defmodule Protean.Interpreter do
  @moduledoc """
  The `Protean.Interpreter` implements the full execution logic for a `Protean.Machine`,
  including handling external events, transitioning state, executing actions, spawning children,
  etc.
  """

  alias __MODULE__
  alias Protean.{Machine, State, Action}

  defstruct [
    :machine,
    :state,
    :handler,
    running: false,
    internal_queue: :queue.new(),
    invoked: %{}
  ]

  @protean_init "$protean_init"

  @type t :: %Interpreter{
          machine: Machine.t(),
          state: State.t(),
          handler: Module.t(),
          running: boolean,
          internal_queue: :queue.queue(),
          invoked: invoked
        }

  @type invoked :: [invoked_service]
  @type invoked_service :: %{
          pid: GenServer.server(),
          autoforward: boolean
        }

  @type options :: [option]
  @type option ::
          {:machine, Machine.t()}
          | {:handler, Module.t()}

  @type metadata :: %{
          state: %{value: State.value()},
          event: Machine.event()
        }

  @type sendable :: Machine.event() | Machine.event_name()

  # SCXML main event loop:
  #
  # 0. if running, continue, otherwise exit interpreter
  # 1. handle automatic transitions
  #   - if any exit, microstep and go to 1
  # 2. handle internal queue
  #   - if event, select transitions, microstep, and go to 1
  # 3. handle invokes
  #   - if states to invoke, invoke them
  #   - if invoking added event to internal queue, go to 1
  # 4. wait for external event
  #   - if termination event received, exit interpreter
  #   - if regular event received, select transitions, microstep, and go to 0

  @doc """
  Create a new `Interpreter` from a `Machine` and handler module. The returned interpreter will
  still need to be started; see `start/1`.
  """
  @spec new(Machine.t(), Module.t()) :: Interpreter.t()
  def new(machine, handler) do
    %Interpreter{
      machine: machine,
      state: Machine.initial_state(machine),
      handler: handler
    }
  end

  @doc "Whether the interpreter has been started and can accept events."
  @spec running?(Interpreter.t()) :: boolean
  def running?(%Interpreter{running: true}), do: true
  def running?(%Interpreter{running: false}), do: false

  @doc """
  Entrypoint for the interpreter that must be called before the interpreter will be in a state
  where it can handle external events. This is necessary in order to handle any initializing
  actions, invokes, or automatic transitions.

  Calling `start/1` on an already-running interpreter is a no-op.
  """
  @spec start(Interpreter.t()) :: Interpreter.t()
  def start(%Interpreter{running: false} = interpreter) do
    %{interpreter | running: true}
    |> add_internal({@protean_init, nil})
    |> run_interpreter()
  end

  def start(interpreter), do: interpreter

  @doc """
  Stop an interpreter, preventing further event processing.
  """
  @spec stop(Interpreter.t()) :: Interpreter.t()
  def stop(%Interpreter{} = interpreter),
    do: %{interpreter | running: false}

  @doc """
  Send an event to a running interpreter. This will execute any transitions, actions, and side-
  effects associated with the current machine state and this event.
  """
  @spec send_event(Interpreter.t(), sendable) :: Interpreter.t()
  def send_event(%Interpreter{running: true} = interpreter, event) when is_binary(event),
    do: send_event(interpreter, {event, nil})

  def send_event(%Interpreter{running: true} = interpreter, event) do
    interpreter
    |> autoforward_event(event)
    |> process_event(event)
  end

  def send_event(interpreter, _event), do: interpreter

  @doc """
  Sets the context of the current state.
  """
  @spec with_context(Interpreter.t(), Machine.context()) :: Interpreter.t()
  def with_context(%Interpreter{} = interpreter, context) do
    put_in(interpreter.state.context, context)
  end

  @doc """
  Updates the context of the current state.
  """
  @spec update_context(Interpreter.t(), (Machine.context() -> Machine.context())) ::
          Interpreter.t()
  def update_context(%Interpreter{} = interpreter, fun) do
    update_in(interpreter.state.context, fun)
  end

  # Entrypoint for the SCXML main event loop. Ensures that any automatic transitions are run and
  # internal events are processed before awaiting an external event.
  defp run_interpreter(%Interpreter{running: true} = interpreter),
    do: run_automatic_transitions(interpreter)

  defp run_interpreter(%Interpreter{running: false} = interpreter),
    do: interpreter

  defp run_automatic_transitions(interpreter) do
    case select_automatic_transitions(interpreter) do
      [] ->
        process_internal_queue(interpreter)

      transitions ->
        transitions
        |> microstep(interpreter)
        |> run_interpreter()
    end
  end

  defp process_internal_queue(%Interpreter{internal_queue: queue} = interpreter) do
    case :queue.out(queue) do
      {:empty, _} ->
        process_invokes(interpreter)

      {{:value, event}, queue} ->
        %{interpreter | internal_queue: queue}
        |> process_event(event)
    end
  end

  defp process_event(interpreter, event) do
    interpreter = set_event(interpreter, event)

    interpreter
    |> select_transitions(event)
    |> microstep(interpreter)
    |> run_interpreter()
  end

  defp set_event(interpreter, event) do
    put_in(interpreter.state.event, event)
  end

  defp process_invokes(interpreter) do
    case select_invokes(interpreter) do
      [] ->
        interpreter

      invokes ->
        interpreter
        |> invoke(invokes)
        |> loop_if_internal_event()
    end
  end

  defp loop_if_internal_event(%{internal_queue: queue} = interpreter) do
    if :queue.is_empty(queue) do
      interpreter
    else
      run_interpreter(interpreter)
    end
  end

  @spec select_automatic_transitions(Interpreter.t()) :: [Transition.t()]
  defp select_automatic_transitions(%{machine: machine, state: state}) do
    Machine.select_automatic_transitions(machine, state)
  end

  @spec select_transitions(Interpreter.t(), Machine.event()) :: [Transition.t()]
  defp select_transitions(%{machine: machine, state: state}, event) do
    Machine.select_transitions(machine, state, event)
  end

  @spec autoforward_event(Interpreter.t(), Machine.event()) :: Interpreter.t()
  defp autoforward_event(%Interpreter{invoked: invoked} = interpreter, event) do
    invoked
    |> Enum.filter(&autoforward?/1)
    |> Enum.reduce(interpreter, &autoforward_to(&1, event, &2))
  end

  @spec autoforward?(invoked_service) :: boolean
  defp autoforward?(%{autoforward: true}), do: true
  defp autoforward?(_invoke), do: false

  @spec autoforward_to(invoked_service, Machine.event(), Interpreter.t()) :: Interpreter.t()
  defp autoforward_to(%{pid: pid}, event, interpreter) do
    # TODO: do we need to handle failures here? can add to internal queue
    # if errors occur.
    Interpreter.Server.send_async(pid, event)
    interpreter
  end

  @spec select_invokes(Interpreter.t()) :: [any]
  defp select_invokes(_interpreter) do
    # TODO
    []
  end

  @spec invoke(Interpreter.t(), [any]) :: Interpreter.t()
  defp invoke(interpreter, _to_invoke) do
    # TODO
    interpreter
  end

  # A microstep fully processes a set of transitions, updating the state configuration and
  # executing any resulting actions.
  defp microstep(transitions, interpreter) do
    %Interpreter{
      machine: machine,
      state: state,
      handler: handler
    } = interpreter

    %State{
      value: value,
      event: event,
      actions: actions,
      context: context
    } = state = Machine.take_transitions(machine, state, transitions)

    meta = %{
      state: %{value: value},
      event: event
    }

    bound_actions = Action.resolve_actions(actions, context, handler, meta)

    interpreter
    |> put_in([Access.key(:state)], state)
    |> exec_all(bound_actions)
    |> put_in([Access.key(:state), Access.key(:actions)], [])
  end

  defp exec_all(interpreter, bound_actions) do
    Enum.reduce(bound_actions, interpreter, &exec_bound_action/2)
  end

  defp exec_bound_action({action, context}, interpreter),
    do: Action.Protocol.Executable.exec(action, context, interpreter)

  defp add_internal(interpreter, event) do
    update_in(interpreter.internal_queue, &:queue.in(event, &1))
  end
end
