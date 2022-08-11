defmodule Protean.Action do
  @moduledoc """
  Protean manages state, processes, and side-effects through **actions**, data structures
  describing things that should occur as a result of a transition.

  ## Introduction

  When a Protean machine transitions from one state to the next (or even self-transitions back
  to the same state), the interpreter collects any actions that should be performed as a result
  of that transition. Actions are then executed in a specific order:

    * Exit actions - Any `:exit` actions specified on states that are exiting as a result of the
      transition.
    * Transition actions - Any `:actions` specified on the transition that responded to the event.
    * Entry actions - Any `:entry` actions specified on the states being entered.

  Actions are a unifying abstraction that serve many purposes:

    * assign to and update a machine's context;
    * produce additional actions to be run immediately after;
    * run arbitrary side-effects, like broadcasting a PubSub message;
    * reply to the sender of the event with a value;
    * spawn additional processes whose lifecycle can be tied to machine execution.

  I recommend reading the sections below to gain a better understanding of how and where actions
  can be applied, then review the "Callback Actions" and "Inline Actions" provided by this module.

  ## Handling an action

  The most common way to use actions is through the `c:Protean.handle_action/3` callback.
  This callback is run when an action is specified like this:

      [
        # ...
        entry: [:entering_state]
        on: [
          {match(%MyEvent{}), actions: [:first_action, :second_action]}
        ],
        exit: [:exiting_state]
      ]

  These are then handled in callbacks:

      @impl true
      def handle_action(:first_action, state, %MyEvent{data: data}) do
        # ...
        {:reply, reply, state}
      end

  See `c:Protean.handle_action/3` for possible return values and their effect.

  Action callbacks must always return the machine state, but they can attach actions to that
  state that will be immediately executed by the interpreter. For instance:

      def handle_action(:update_data, state, {:data_updated, changes}) do
        state
        |> Action.assign_in([:data], &Map.merge(&1, changes))
      end

  In this case, `assign_in/3` is being used to update some data in the machine `:assigns`.
  But, we could perform additional actions if we wish, such as:

      def handle_action(:update_data, state, {:data_updated, changes}) do
        %{topic: topic, other_process: pid, data: data} = state.assigns
        new_data = Map.merge(data, changes)

        PubSub.broadcast!(@pubsub, topic, {:data_updated, changes})

        state
        |> Action.send({:data_commited, new_data}, to: pid)
        |> Action.assign(:data, new_data)
      end

  > #### Note about return values {: .tip}
  >
  > These last two examples have returned the state directly, which is equivalent to returning
  > `{:noreply, state}`. You can also emit replies that will be available to the sender of the
  > event using `{:reply, reply, state}`. This can be very useful for creating client APIs around
  > your machines, much like you would do with `GenServer`.

  The best practice is to let the interpreter execute as many actions as possible, as opposed to
  performing explicit side-effects in the callback. In many cases, such as spawning additional
  processes, this allows Protean to supervise and tie process lifecycle to the machine's
  lifecycle (or changes in state).

  ## Low-level API: the Action behaviour

  Ultimately, all actions resolve to `t:Protean.Action.t/0`, a simple struct containing a module
  and an argument. The module is expected to implement the `Protean.Action` behaviour.

  Action execution, then, follows a simple logic: if the action being executed is a `Protean.Action` struct, call the `c:exec_action/2` callback on the module, passing it the associated
  argument and the interpreter in its current state. Otherwise, wrap it in a struct that
  delegates to the callback module associated with the machine. This is how
  `c:Protean.handle_action/3` is called.

  Many of features features boil down to syntax sugar over actions, including `:invoke` and
  delayed transitions using `:after`.
  """

  import Kernel, except: [send: 2]

  alias __MODULE__
  alias Protean.Events
  alias Protean.Guard
  alias Protean.Interpreter
  alias Protean.Context

  @enforce_keys [:module, :arg]
  defstruct [:module, :arg]

  @typedoc "Data structure representing an executable action."
  @type t :: %Action{
          module: module(),
          arg: term()
        }

  @typedoc "Allowed return value from `c:exec_action/2`."
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

  @doc false
  @spec new(module(), term()) :: t
  def new(module, arg), do: %Action{module: module, arg: arg}

  defp new(arg), do: new(__MODULE__, arg)

  @doc false
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

  @doc """
  Attach a custom action that implements the `Protean.Action` behaviour.
  """
  @doc type: :callback_action
  @spec put(Context.t(), t) :: Context.t()
  def put(%Context{} = state, %Action{} = action), do: put_action(action, state)

  @doc false
  def delegate(%Context{} = state, action), do: delegate(action) |> put_action(state)

  @doc false
  def delegate(action) do
    new({:delegate, action})
  end

  @doc """
  Attach an action that assigns to a machine's context.
  """
  @doc type: :callback_action
  @spec assign(Context.t(), key :: term(), value :: term()) :: Context.t()
  @spec assign(Context.t(), assigns :: term()) :: Context.t()
  def assign(%Context{} = state, key, value), do: assign(key, value) |> put_action(state)
  def assign(%Context{} = state, assigns), do: assign(assigns) |> put_action(state)

  @doc """
  Create an inline action that will assign to machine context.
  """
  @doc type: :inline_action
  @spec assign(key :: term(), value :: term()) :: t
  def assign(key, value) do
    new({:assign, :merge, %{key => value}})
  end

  @doc """
  Create an inline action that applies a function to or merges assigns into machine context.
  """
  @doc type: :inline_action
  @spec assign(fun :: function()) :: t
  @spec assign(assigns :: term()) :: t
  def assign(fun) when is_function(fun) do
    new({:assign, :update, fun})
  end

  def assign(assigns) do
    new({:assign, :merge, Enum.into(assigns, %{})})
  end

  @doc """
  Attach an action that assigns into a machine's context.

  Similar to `put_in/3`.
  """
  @doc type: :callback_action
  @spec assign_in(Context.t(), [term(), ...], term()) :: Context.t()
  def assign_in(%Context{} = state, path, value), do: assign_in(path, value) |> put_action(state)

  @doc """
  Create an inline action that will assign into machine context.

  Similar to `put_in/3`.
  """
  @doc type: :inline_action
  @spec assign_in([term(), ...], function()) :: t
  @spec assign_in([term(), ...], term()) :: t
  def assign_in(path, fun) when is_list(path) and is_function(fun) do
    assign(fn %{assigns: assigns} -> update_in(assigns, path, fun) end)
  end

  def assign_in(path, value) when is_list(path) do
    assign(fn %{assigns: assigns} -> put_in(assigns, path, value) end)
  end

  @doc """
  Attach an action that will send a message to a process.
  """
  @doc type: :callback_action
  @spec send(Context.t(), event :: term(), [term()]) :: Context.t()
  def send(%Context{} = state, event, opts) do
    send(event, opts) |> put_action(state)
  end

  @doc """
  Create an inline action that will send a message to a process.
  """
  @doc type: :inline_action
  @spec send(event :: term(), [term()]) :: t
  def send(event, opts \\ []) do
    if delay = opts[:delay] do
      new({:send_after, event, opts[:to], delay})
    else
      new({:send, event, opts[:to]})
    end
  end

  @doc """
  Attach an action that executes the first of a list of actions whose guard is truthy.
  """
  @doc type: :callback_action
  def choose(%Context{} = state, actions), do: choose(actions) |> put_action(state)

  @doc """
  Create an inline action that will execute the first of a list of actions whose guard is truthy.
  """
  @doc type: :inline_action
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

  @doc false
  def invoke(:cancel, id) do
    new({:invoke, :cancel, id})
  end

  # Action callbacks

  @doc false
  def exec_action({:delegate, action}, interpreter) do
    %{state: state, config: config} = interpreter

    case config.callback_module.handle_action(action, state, state.event) do
      {:noreply, state} ->
        {:cont, interpreter, Context.actions(state)}

      {:reply, reply, state} ->
        {:cont, Interpreter.put_reply(interpreter, reply), Context.actions(state)}

      %Context{} = state ->
        {:cont, interpreter, Context.actions(state)}

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
    {:cont, %{interpreter | state: Context.assign(state, assigns)}}
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

  def exec_action({:invoke, :delayed_send, name, id, opts}, interpreter)
      when not is_integer(name) do
    delay = run_callback(:delay, name, interpreter)
    exec_action({:invoke, :delayed_send, delay, id, opts}, interpreter)
  end

  def exec_action({:invoke, invoke_type, name, id, opts}, interpreter) when is_binary(name) do
    to_invoke = run_callback(:invoke, name, interpreter)
    exec_action({:invoke, invoke_type, to_invoke, id, opts}, interpreter)
  end

  def exec_action({:invoke, :delayed_send, delay, id, opts}, interpreter)
      when is_integer(delay) do
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
    Context.put_actions(state, [action])
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
