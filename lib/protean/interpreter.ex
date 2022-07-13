defmodule Interpreter do
  @moduledoc """
  The `Protean.Interpreter` is a `GenServer` process primarily responsible for interpreting
  and executing a `Protean.Machine`. This includes accepting incoming events, executing
  transitions and actions, spawning child machines/processes, etc.

  This module is usually not invoked used directly, but instead defines the behavior that
  is used by modules that `use Protean`. See `Protean` for client API.
  """

  use GenServer

  alias __MODULE__
  alias Protean.{Machine, State, Action, Action.Executable}

  @protean_event :"$protean.event"
  @protean_terminate :"$protean.terminate"
  @protean_snapshot :"$protean.snapshot"

  defstruct [
    :machine,
    :state,
    :handler,
    running: true,
    internal_queue: :queue.new(),
    invoked: %{}
  ]

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
  @type option :: {:handler, Module.t()}

  @type metadata :: %{
          state: %{value: State.value()},
          event: Machine.event()
        }

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
  Entrypoint for the interpreter, ensuring that any automatic transitions are run,
  internal events handled, invokes invoked, etc. before waiting for an external event.
  """
  def run_interpreter(%Interpreter{running: true} = interpreter),
    do: run_automatic_transitions(interpreter)

  def run_interpreter(%Interpreter{running: false} = interpreter),
    do: exit_interpreter(interpreter)

  @doc """
  Handler for external event with "run-to-completion" semantics: ensures that the
  interpreter is either ready to receive the next event or is no longer running.
  """
  def handle_event(interpreter, event) do
    interpreter
    |> autoforward_event(event)
    |> process_event(event)
  end

  def run_automatic_transitions(interpreter) do
    case select_automatic_transitions(interpreter) do
      [] ->
        process_internal_queue(interpreter)

      transitions ->
        transitions
        |> microstep(interpreter)
        |> run_interpreter()
    end
  end

  def process_internal_queue(%Interpreter{internal_queue: queue} = interpreter) do
    case :queue.out(queue) do
      {:empty, _} ->
        process_invokes(interpreter)

      {{:value, event}, queue} ->
        %{interpreter | internal_queue: queue}
        |> process_event(event)
    end
  end

  def process_event(interpreter, event) do
    interpreter
    |> select_transitions(event)
    |> microstep(interpreter)
    |> run_interpreter()
  end

  def process_invokes(interpreter) do
    case select_invokes(interpreter) do
      [] ->
        interpreter

      invokes ->
        interpreter
        |> invoke(invokes)
        |> loop_if_internal_event()
    end
  end

  def loop_if_internal_event(%{internal_queue: queue} = interpreter) do
    if :queue.is_empty(queue) do
      interpreter
    else
      run_interpreter(interpreter)
    end
  end

  @spec select_automatic_transitions(Interpreter.t()) :: [Transition.t()]
  def select_automatic_transitions(%{machine: machine, state: state}) do
    Machine.select_automatic_transitions(machine, state)
  end

  @spec select_transitions(Interpreter.t(), Machine.event()) :: [Transition.t()]
  def select_transitions(%{machine: machine, state: state}, event) do
    Machine.select_transitions(machine, state, event)
  end

  @spec autoforward_event(Interpreter.t(), Machine.event()) :: Interpreter.t()
  def autoforward_event(%Interpreter{invoked: invoked} = interpreter, event) do
    invoked
    |> Enum.filter(&autoforward?/1)
    |> Enum.reduce(interpreter, &autoforward_to(&1, event, &2))
  end

  @spec autoforward?(invoked_service) :: boolean
  def autoforward?(%{autoforward: true}), do: true
  def autoforward?(_invoke), do: false

  @spec autoforward_to(invoked_service, Machine.event(), Interpreter.t()) :: Interpreter.t()
  def autoforward_to(%{pid: pid}, event, interpreter) do
    # TODO: do we need to handle failures here? can add to internal queue
    # if errors occur.
    send_async(pid, event)
    interpreter
  end

  @spec select_invokes(Interpreter.t()) :: [any]
  def select_invokes(_interpreter) do
    # TODO
    []
  end

  @spec invoke(Interpreter.t(), [any]) :: Interpreter.t()
  def invoke(interpreter, _to_invoke) do
    # TODO
    interpreter
  end

  @doc """
  A `microstep` processes a single set of transitions, updating the state configuration
  and executing the resulting actions.
  """
  def microstep(transitions, interpreter)

  def microstep([], interpreter), do: interpreter

  def microstep(transitions, interpreter) do
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
    } = state = Machine.take_transition(machine, state, transitions)

    interpreter = %{interpreter | state: state}

    meta = %{
      state: %{value: value},
      event: event
    }

    bound_actions = Action.resolve_actions(actions, context, handler, meta)

    Enum.reduce(bound_actions, interpreter, fn
      {action, context}, interpreter ->
        Executable.exec(action, context, interpreter)
    end)
  end

  def exit_interpreter(interpreter),
    do: interpreter

  # Client

  @spec start_link(Machine.t(), Interpreter.options(), GenServer.options()) ::
          GenServer.on_start()
  def start_link(machine, interpreter_opts, server_opts \\ []) do
    GenServer.start_link(__MODULE__, {machine, interpreter_opts}, server_opts)
  end

  @doc """
  Send an event to the interpreter and wait for the next state.
  """
  @spec send(GenServer.server(), Machine.event()) :: State.t()
  def send(pid, event) do
    GenServer.call(pid, {@protean_event, event})
  end

  @doc """
  Send an event to the interpreter asyncronously.
  """
  @spec send_async(GenServer.server(), Machine.event()) :: :ok
  def send_async(pid, event) do
    GenServer.cast(pid, {@protean_event, event})
  end

  @doc """
  Get a snapshot of the current machine state.
  """
  @spec snapshot(GenServer.server()) :: State.t()
  def snapshot(pid) do
    GenServer.call(pid, @protean_snapshot)
  end

  # Server (callbacks)

  @impl GenServer
  def init({machine, opts}) do
    interpreter = %Interpreter{
      machine: machine,
      state: Machine.initial_state(machine),
      handler: Keyword.fetch!(opts, :handler)
    }

    {:ok, interpreter}
  end

  @impl GenServer
  def handle_call(@protean_snapshot, _from, interpreter) do
    {:reply, interpreter.state, interpreter}
  end

  @impl GenServer
  def handle_call({@protean_event, event}, _from, interpreter) do
    interpreter = handle_event(interpreter, event)
    {:reply, interpreter.state, interpreter}
  end

  @impl GenServer
  def handle_cast({@protean_event, event}, interpreter) do
    {:noreply, interpreter, {:continue, {@protean_event, event}}}
  end

  @impl GenServer
  def handle_continue({@protean_event, event}, interpreter) do
    {:noreply, handle_event(interpreter, event)}
  end
end
