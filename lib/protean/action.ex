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
          match(%MyEvent{}, actions: [:first_action, :second_action])
        ],
        exit: [:exiting_state]
      ]

  These are then handled in callbacks:

      @impl true
      def handle_action(:first_action, context, %MyEvent{data: data}) do
        # ...
        {:reply, reply, context}
      end

  See `c:Protean.handle_action/3` for possible return values and their effect.

  Action callbacks must always return the machine context, but they can attach actions to that
  context that will be immediately executed by the interpreter. For instance:

      def handle_action(:update_data, context, {:data_updated, changes}) do
        context
        |> Action.update_in([:data], &Map.merge(&1, changes))
      end

  In this case, `update_in/3` is being used to update some data in the machine `:assigns`.
  But, we could perform additional actions if we wish, such as:

      def handle_action(:update_data, context, {:data_updated, changes}) do
        %{topic: topic, other_process: pid, data: data} = context.assigns
        new_data = Map.merge(data, changes)

        PubSub.broadcast!(@pubsub, topic, {:data_updated, changes})

        context
        |> Action.send({:data_commited, new_data}, to: pid)
        |> Action.assign(:data, new_data)
      end

  > #### Note about return values {: .tip}
  >
  > These last two examples have returned the context directly, which is equivalent to returning
  > `{:noreply, context}`. You can also emit replies that will be available to the sender of the
  > event using `{:reply, reply, context}`. This can be very useful for creating client APIs around
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

  Many of features features boil down to syntax sugar over actions, including `:spawn` and
  delayed transitions using `:after`.
  """

  import Kernel, except: [send: 2, update_in: 2, update_in: 3]

  alias __MODULE__
  alias Protean.Context
  alias Protean.Events
  alias Protean.Guard
  alias Protean.Interpreter
  alias Protean.ProcessManager

  require Logger

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

  @doc false
  @spec exec_all(Interpreter.t(), [t | term()]) :: Interpreter.t()
  def exec_all(interpreter, [action | rest]) do
    case exec(action, interpreter) do
      {:halt, interpreter} -> interpreter
      {:cont, interpreter} -> exec_all(interpreter, rest)
      {:cont, interpreter, actions} -> exec_all(interpreter, actions ++ rest)
    end
  end

  def exec_all(interpreter, []), do: interpreter

  @doc """
  Attach a custom action that implements the `Protean.Action` behaviour.
  """
  @doc type: :callback_action
  @spec put(Context.t(), t) :: Context.t()
  def put(%Context{} = context, %Action{} = action), do: put_action(action, context)

  @doc false
  def delegate(%Context{} = context, action), do: delegate(action) |> put_action(context)

  @doc false
  def delegate(action) do
    new({:delegate, action})
  end

  @doc """
  Attach an action that assigns a value to a key in a machine's context.
  """
  @doc type: :callback_action
  @spec assign(Context.t(), key :: term(), value :: term()) :: Context.t()
  def assign(%Context{} = context, key, value), do: assign([{key, value}]) |> put_action(context)

  @doc """
  Attach an action that merges the given assigns into a machine's context.
  """
  @doc type: :callback_action
  @spec assign(Context.t(), assigns :: Enumerable.t()) :: Context.t()
  def assign(%Context{} = context, assigns), do: assign(assigns) |> put_action(context)

  @doc """
  Same as `assign/2` but used inline in machine configuration.
  """
  @doc type: :inline_action
  @spec assign(Enumerable.t()) :: t
  def assign(assigns) do
    new({:assign, :merge, Map.new(assigns)})
  end

  @doc """
  Attach an action that assigns into a machine's context.

  Similar to `Kernel.put_in/3`.
  """
  @doc type: :callback_action
  @spec assign_in(Context.t(), [term(), ...], term()) :: Context.t()
  def assign_in(%Context{} = context, path, value),
    do: assign_in(path, value) |> put_action(context)

  @doc """
  Same as `assign_in/3` but used inline in machine configuration.
  """
  @doc type: :inline_action
  @spec assign_in([term(), ...], term()) :: t
  def assign_in(path, value) when is_list(path) do
    update(fn assigns -> put_in(assigns, path, value) end)
  end

  @doc """
  Attach an action that calls a function with a machine's context and merges the result into
  assigns.
  """
  @doc type: :callback_action
  def update(%Context{} = context, fun), do: update(fun) |> put_action(context)

  @doc """
  Same as `update/2`, but inserted inline in machine config.
  """
  @doc type: :inline_action
  def update(fun) when is_function(fun) do
    new({:assign, :update, fun})
  end

  @doc """
  Attach an action that applies an update function into a machine's context.

  Similar to `Kernel.update_in/3`.
  """
  @doc type: :callback_action
  @spec update_in(Context.t(), [term(), ...], function()) :: Context.t()
  def update_in(%Context{} = context, path, fun) do
    update_in(path, fun) |> put_action(context)
  end

  @doc """
  Same as `update_in/3` but used inline in machine configuration.
  """
  @doc type: :inline_action
  @spec update_in([term(), ...], function()) :: t
  def update_in(path, fun) when is_list(path) and is_function(fun) do
    update(fn assigns -> Kernel.update_in(assigns, path, fun) end)
  end

  @doc """
  Attach an action that will send a message to a process.
  """
  @doc type: :callback_action
  @spec send(Context.t(), event :: term(), [term()]) :: Context.t()
  def send(%Context{} = context, event, opts) do
    send(event, opts) |> put_action(context)
  end

  @doc """
  Same as `send/3` but used inline in machine configuration.
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
  def choose(%Context{} = context, actions), do: choose(actions) |> put_action(context)

  @doc """
  Same as `choose/2` but used inline in machine configuration.
  """
  @doc type: :inline_action
  def choose(actions) when is_list(actions) do
    new({:choose, actions})
  end

  @doc false
  def spawn(type, to_spawn, id, opts \\ [])

  def spawn(:proc, proc, id, opts) do
    new({:spawn, :proc, proc, id, opts})
  end

  def spawn(:task, task, id, opts) do
    new({:spawn, :task, task, id, opts})
  end

  def spawn(:delayed_send, delay, id, opts) do
    new({:spawn, :delayed_send, delay, id, opts})
  end

  def spawn(:stream, stream, id, opts) do
    new({:spawn, :stream, stream, id, opts})
  end

  @doc false
  def spawn(:cancel, id) do
    new({:spawn, :cancel, id})
  end

  # Action callbacks

  @doc false
  def exec_action({:delegate, action}, interpreter) do
    %{context: context, config: config} = interpreter

    case config.callback_module.handle_action(action, context, context.event) do
      {:noreply, context} ->
        {:cont, interpreter, Context.actions(context)}

      {:reply, reply, context} ->
        {:cont, Interpreter.put_reply(interpreter, reply), Context.actions(context)}

      %Context{} = context ->
        {:cont, interpreter, Context.actions(context)}

      other ->
        Logger.error("""
        Received invalid return value from action callback. Expected one of:

          {:noreply, context}
          {:reply, reply, context}

        Got:

          #{inspect(other)}
        """)

        {:cont, interpreter}
    end
  end

  def exec_action({:assign, :merge, assigns}, interpreter) do
    %{context: context} = interpreter
    {:cont, %{interpreter | context: Context.assign(context, assigns)}}
  end

  def exec_action({:assign, :update, fun}, interpreter) do
    assigns = interpreter.context.assigns
    exec_action({:assign, :merge, fun.(assigns)}, interpreter)
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

  def exec_action({:spawn, :cancel, id}, interpreter) do
    ProcessManager.stop_subprocess(id)
    {:cont, interpreter}
  end

  def exec_action({:spawn, :delayed_send, delay, id, _opts}, interpreter) do
    delay =
      case delay do
        delay when is_integer(delay) -> delay
        other -> run_callback(:delay, [other], interpreter)
      end

    f = fn -> :timer.sleep(delay) end

    spawn_task(interpreter, id, f)
  end

  def exec_action({:spawn, :stream, stream, id, _opts}, interpreter) do
    stream =
      case stream do
        name when is_atom(name) or is_binary(name) ->
          run_callback(:spawn, [:stream, name], interpreter)

        stream ->
          stream
      end

    self = self()

    task = fn ->
      for event <- stream, do: Kernel.send(self, event)
      :ok
    end

    spawn_task(interpreter, id, task)
  end

  def exec_action({:spawn, :task, task, id, _opts}, interpreter) do
    task =
      case task do
        {_m, _f, _a} = mfa -> mfa
        f when is_function(f) -> f
        other -> run_callback(:spawn, [:task, other], interpreter)
      end

    spawn_task(interpreter, id, task)
  end

  def exec_action({:spawn, :proc, proc, id, opts}, interpreter) do
    proc =
      case proc do
        {mod, arg} -> {mod, Keyword.put_new(arg, :parent, self())}
        mod -> {mod, parent: self()}
      end
      |> Supervisor.child_spec(restart: :temporary)

    spawn_proc(interpreter, id, proc, opts)
  end

  defp spawn_proc(interpreter, id, proc, opts) do
    with :ok <- ProcessManager.start_subprocess(id, proc, opts) do
      {:cont, interpreter}
    else
      error -> spawn_error(interpreter, id, proc, error)
    end
  end

  defp spawn_task(interpreter, id, task) do
    with :ok <- ProcessManager.start_task(id, task) do
      {:cont, interpreter}
    else
      error -> spawn_error(interpreter, id, task, error)
    end
  end

  defp spawn_error(interpreter, id, spawned, error) do
    Logger.warn("spawn #{inspect(spawned)} failed to start with error #{inspect(error)}")
    {:cont, Interpreter.add_internal(interpreter, Events.platform(:spawn, :error, id))}
  end

  defp guard_allows?({_, guard: guard}, interpreter) do
    %{context: context, config: config} = interpreter
    Guard.allows?(guard, context, context.event, config.callback_module)
  end

  defp guard_allows?(_, _), do: true

  defp normalize_choice({action, _}), do: action
  defp normalize_choice(action), do: action

  defp put_action(action, context) do
    Context.put_actions(context, [action])
  end

  defp resolve_recipient(_interpreter, nil), do: self()
  defp resolve_recipient(_interpreter, :self), do: self()
  defp resolve_recipient(%{parent: parent}, :parent), do: parent

  defp resolve_recipient(_interpreter, maybe_id) do
    case ProcessManager.fetch_subprocess(maybe_id) do
      {:ok, {_, pid, _, _}} -> pid
      :error -> maybe_id
    end
  end

  defp run_callback(callback_name, args, interpreter) when is_list(args) do
    %{context: context, config: config} = interpreter

    apply(config.callback_module, callback_name, args ++ [context, context.event])
  end
end
