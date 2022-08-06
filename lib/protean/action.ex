defmodule Protean.Action do
  @moduledoc """
  Protean manages state, processes, and side-effects through **actions**, data structures
  describing things that should occur as a result of a transition.

  When a Protean machine transitions from one state to the next (or even "self-transitions" back
  to the same state), the interpreter collects any actions that should be performed as a result
  of that transition. Actions are then executed in a specific order:

  * Exit actions - Any `:exit` actions specified on states that are exiting as a result of the
    transition.
  * Transition actions - Any `:actions` specified on the transition that responded to the event.
  * Entry actions - Any `:entry` actions specified on the states being entered.

  The execution of an action can change the state of the machine (it's context, for example), or
  run some side-effect (like broadcasting a PubSub message). Action execution can additionally
  produce more actions that will be executed immediately before moving onto the next top-level
  action as described above.

  ## High-level API

  The most common way to use actions is through the `c:Protean.action/3` callback. This callback
  is run when an action is specified like this:

      [
        # ...
        on: [
          {:some_event, actions: [:first_action, :second_action]}
        ]
      ]

  These are then handled in callbacks:

      @impl Protean
      def action(:first_action, state, _event) do
        # ...
        state
      end

      def action(:second_action, state, _event) do
        # ...
        state
      end

  Action callbacks must always return the machine state, but they can attach actions to that
  state that will be immediately executed by the interpreter. For instance:

      def action(:update_data, state, {:data_updated, changes}) do
        state
        |> Action.assign_in([:data], &Map.merge(&1, changes))
      end

  In this case, `assign_in/3` is being used to update some data in the machine `:context`.
  But, we could perform additional actions if we wish, such as:

      def action(:update_data, state, {:data_updated, changes}) do
        %{topic: topic, other_process: pid, data: data} = state.context
        new_data = Map.merge(data, changes)

        PubSub.broadcast!(@pubsub, topic, {:data_updated, changes})

        state
        |> Action.send({:data_commited, new_data}, to: pid)
        |> Action.assign(:data, new_data)
      end

  Note: Best practice is to let the interpreter execute as many actions as possible, as opposed to
  performing explicit side-effects in the callback. This is because actions can potentially halt
  the execution of any remaining actions, but side-effects executed in the callback will occur
  before any of the returned actions can be executed.

  `Protean.Action` provides a number of helpers for specifying actions. See individual function
  documentation for more.

  ## Low-level API: the Action behaviour

  Ultimately, all actions resolve to `t:Protean.Action.t()`, a simple struct containing a module
  and an argument. The module is expected to implement the `Protean.Action` behaviour.

  Action execution, then, follows a simple logic: if the action being executed is an `%Action{}`,
  then call the `c:exec_action/2` callback on the module, passing it the associated argument and
  the interpreter in its current state. If it is not an `%Action{}`, wrap it in one that
  automatically delegates to the callback module associated with the machine. This is where
  `c:Protean.action/3` is called.

  Many of Protean's more "dynamic" features boil down to syntax sugar over actions, including
  `:invoke` and delayed transitions using `:after`. Modules implementing the action behaviour
  have direct access to the interpreter and therefore must be careful, well, not to muck anything
  up.
  """

  import Kernel, except: [send: 2]

  alias __MODULE__
  alias Protean.Events
  alias Protean.Guard
  alias Protean.Interpreter
  alias Protean.State

  @enforce_keys [:module, :arg]
  defstruct [:module, :arg]

  @typedoc "Data structure representing an executable action."
  @type t :: %Action{
          module: module(),
          arg: term()
        }

  @type exec_action_return ::
          {:cont, Interpreter.t()}
          | {:cont, Interpreter.t(), [t]}
          | {:halt, Interpreter.t()}

  @doc """
  Accepts the `action_arg` passed with the action as well as the Protean interpreter. Returns
  one of three values:

    * `{:cont, interpreter}` to continue running the action pipeline
    * `{:cont, interpreter, [action]}` to inject actions that will be run immediately before
      running the rest of the pipeline's actions
    * `{:halt, interpreter}` to halt the action pipeline, canceling any further actions
  """
  @callback exec_action(action_arg :: term(), Interpreter.t()) :: exec_action_return

  @doc """
  Create a new Protean action.

  `module` is expected to implement the `Protean.Action` behaviour.
  """
  @spec new(module(), term()) :: t
  def new(module, arg), do: %Action{module: module, arg: arg}

  defp new(arg), do: new(__MODULE__, arg)

  @doc "Executes an action given an interpreter. See `c:exec_action/2`."
  @spec exec(t | term(), Interpreter.t()) :: exec_action_return
  def exec(%Action{module: module, arg: arg}, interpreter) do
    case module.exec_action(arg, interpreter) do
      {:cont, interpreter, []} -> {:cont, interpreter}
      {:cont, interpreter, actions} when is_list(actions) -> {:cont, interpreter, actions}
      {:cont, interpreter} -> {:cont, interpreter}
      {:halt, interpreter} -> {:halt, interpreter}
      other -> raise "Unknown return from #{inspect(module)}.exec_action/2: #{inspect(other)}"
    end
  end

  def exec(action, interpreter) do
    action
    |> delegate()
    |> exec(interpreter)
  end

  @doc "TODO"
  @doc type: :action
  @spec put(State.t(), t) :: State.t()
  def put(%State{} = state, %Action{} = action), do: put_action(action, state)

  @doc "TODO"
  @doc type: :action
  def delegate(%State{} = state, action), do: delegate(action) |> put_action(state)

  def delegate(action) do
    new({:delegate, action})
  end

  @doc "TODO"
  def assign(%State{} = state, key, value), do: assign(key, value) |> put_action(state)
  def assign(%State{} = state, assigns), do: assign(assigns) |> put_action(state)

  def assign(key, value) do
    new({:assign, :merge, %{key => value}})
  end

  def assign(fun) when is_function(fun) do
    new({:assign, :update, fun})
  end

  def assign(assigns) do
    new({:assign, :merge, Enum.into(assigns, %{})})
  end

  @doc "TODO"
  def assign_in(%State{} = state, path, value), do: assign_in(path, value) |> put_action(state)

  def assign_in(path, fun) when is_list(path) and is_function(fun) do
    assign(fn %{context: context} -> update_in(context, path, fun) end)
  end

  def assign_in(path, value) when is_list(path) do
    assign(fn %{context: context} -> put_in(context, path, value) end)
  end

  @doc "TODO"
  def send(%State{} = state, event, opts) do
    send(event, opts) |> put_action(state)
  end

  def send(event, opts \\ []) do
    if delay = opts[:delay] do
      new({:send_after, event, opts[:to], delay})
    else
      new({:send, event, opts[:to]})
    end
  end

  @doc "TODO"
  def choose(%State{} = state, actions), do: choose(actions) |> put_action(state)

  def choose(actions) when is_list(actions) do
    new({:choose, actions})
  end

  @doc false
  def invoke(type, to_invoke, id, opts \\ [])

  def invoke(:proc, proc, id, opts) do
    new({:invoke, :proc, proc, id, opts})
  end

  def invoke(:task, task, id, opts) do
    new({:invoke, :task, task, id, opts})
  end

  def invoke(:delayed_send, delay, id, opts) do
    new({:invoke, :delayed_send, delay, id, opts})
  end

  def invoke(:stream, stream, id, opts) do
    new({:invoke, :stream, stream, id, opts})
  end

  def invoke(:cancel, id) do
    new({:invoke, :cancel, id})
  end

  # Action callbacks

  def exec_action({:delegate, action}, interpreter) do
    %{state: state, config: config} = interpreter

    case config.callback_module.action(action, state, state.event) do
      {:noreply, state} ->
        {:cont, interpreter, State.actions(state)}

      {:reply, reply, state} ->
        {:cont, Interpreter.put_reply(interpreter, reply), State.actions(state)}

      %State{} = state ->
        {:cont, interpreter, State.actions(state)}

      other ->
        require Logger

        Logger.error("""
        Received invalid return value from action callback. Expected one of:

          {:noreply, state}
          {:reply, reply, state}

        Got:

          #{inspect(other)}
        """)

        {:cont, interpreter}
    end
  end

  def exec_action({:assign, :merge, assigns}, interpreter) do
    %{state: state} = interpreter
    {:cont, %{interpreter | state: State.assign(state, assigns)}}
  end

  def exec_action({:assign, :update, fun}, interpreter) do
    %{state: state} = interpreter

    assigns =
      case Function.info(fun, :arity) do
        {_, 0} -> fun.()
        {_, 1} -> fun.(state)
        {_, 2} -> fun.(state, state.event)
      end

    exec_action({:assign, :merge, assigns}, interpreter)
  end

  def exec_action({:send, event, to}, interpreter) do
    interpreter
    |> resolve_recipient(to)
    |> Protean.send(event)

    {:cont, interpreter}
  end

  def exec_action({:send_after, event, to, delay}, interpreter) do
    interpreter
    |> resolve_recipient(to)
    |> Protean.send_after(event, delay)

    {:cont, interpreter}
  end

  def exec_action({:choose, actions}, interpreter) do
    choice = actions |> Enum.find(&guard_allows?(&1, interpreter)) |> normalize_choice()
    {:cont, interpreter, List.wrap(choice)}
  end

  def exec_action({:invoke, :cancel, id}, interpreter) do
    interpreter =
      case interpreter.invoked[id] do
        %{pid: pid, ref: ref} ->
          Protean.Supervisor.terminate_child(interpreter.supervisor, pid)
          Process.demonitor(ref, [:flush])
          update_in(interpreter.invoked, &Map.delete(&1, id))

        _ ->
          interpreter
      end

    {:cont, interpreter}
  end

  def exec_action({:invoke, :delayed_send, name, id, opts}, interpreter) when is_binary(name) do
    delay = run_callback(:delay, name, interpreter)
    exec_action({:invoke, :delayed_send, delay, id, opts}, interpreter)
  end

  def exec_action({:invoke, invoke_type, name, id, opts}, interpreter) when is_binary(name) do
    to_invoke = run_callback(:invoke, name, interpreter)
    exec_action({:invoke, invoke_type, to_invoke, id, opts}, interpreter)
  end

  def exec_action({:invoke, :delayed_send, delay, id, opts}, interpreter) do
    f = fn ->
      :timer.sleep(delay)
      id
    end

    exec_action({:invoke, :function, f, id, opts}, interpreter)
  end

  def exec_action({:invoke, :proc, proc, id, opts}, interpreter) do
    child_spec_fun = fn pid ->
      defaults = [parent: pid]

      case proc do
        {mod, arg} -> {mod, Keyword.merge(defaults, arg)}
        mod -> {mod, defaults}
      end
      |> Supervisor.child_spec(restart: :temporary)
    end

    __invoke__(id, child_spec_fun, interpreter, opts)
  end

  def exec_action({:invoke, :task, task, id, opts}, interpreter) do
    on_done = Events.platform(:invoke, :done, id)

    child_spec_fun = fn pid ->
      Task.child_spec(fn -> Kernel.send(pid, {on_done, run_task(task)}) end)
    end

    __invoke__(id, child_spec_fun, interpreter, opts)
  end

  def exec_action({:invoke, :stream, stream, id, opts}, interpreter) do
    on_done = Events.platform(:invoke, :done, id)

    child_spec_fun = fn pid ->
      Task.child_spec(fn ->
        for event <- stream, do: Kernel.send(pid, event)
        Kernel.send(pid, {on_done, nil})
      end)
    end

    __invoke__(id, child_spec_fun, interpreter, opts)
  end

  def exec_action({:invoke, :function, f, id, opts}, interpreter) when is_function(f) do
    child_spec_fun = fn pid ->
      Task.child_spec(fn -> Kernel.send(pid, f.()) end)
    end

    __invoke__(id, child_spec_fun, interpreter, opts)
  end

  defp __invoke__(id, child_spec_fun, interpreter, opts) do
    self_alias = :erlang.alias()

    Protean.Supervisor.start_child(
      interpreter.supervisor,
      child_spec_fun.(self_alias)
    )
    |> case do
      {:ok, child} ->
        ref = Process.monitor(child)

        update_in(
          interpreter.invoked,
          &Map.put(&1, id, %{
            id: id,
            pid: child,
            ref: ref,
            autoforward: Keyword.get(opts, :autoforward, false),
            interpreter_alias: self_alias
          })
        )

      {:error, reason} ->
        Interpreter.notify_process_down(interpreter, reason, id: id)
    end
    |> then(&{:cont, &1})
  end

  defp guard_allows?({_, guard: guard}, interpreter) do
    %{state: state, config: config} = interpreter
    Guard.allows?(guard, state, state.event, config.callback_module)
  end

  defp guard_allows?(_, _), do: true

  defp normalize_choice({action, _}), do: action
  defp normalize_choice(action), do: action

  defp put_action(action, state) do
    State.put_actions(state, [action])
  end

  defp resolve_recipient(_interpreter, nil), do: self()
  defp resolve_recipient(_interpreter, :self), do: self()
  defp resolve_recipient(%{parent: parent}, :parent), do: parent

  defp resolve_recipient(interpreter, name) when is_binary(name),
    do: interpreter.invoked[name][:pid]

  defp resolve_recipient(_interpreter, to), do: to

  defp run_task({m, f, a}), do: apply(m, f, a)
  defp run_task(f) when is_function(f), do: f.()

  defp run_callback(callback_name, arg, interpreter) do
    %{state: state, config: config} = interpreter

    apply(config.callback_module, callback_name, [arg, state, state.event])
  end
end
