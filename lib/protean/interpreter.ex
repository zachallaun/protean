defmodule Protean.Interpreter do
  @moduledoc """
  The `Protean.Interpreter` implements the full execution logic for a `Protean.Machine`,
  including handling external events, transitioning state, executing actions, spawning children,
  etc.
  """

  alias __MODULE__
  alias Protean.Action
  alias Protean.Machine
  alias Protean.State
  alias Protean.Utils

  defstruct [
    :machine,
    :state,
    :handler,
    :parent,
    :supervisor,
    running: false,
    internal_queue: :queue.new(),
    invoked: %{},
    subscribed: []
  ]

  @type t :: %Interpreter{
          machine: Machine.t(),
          state: State.t(),
          handler: module(),
          parent: pid(),
          supervisor: Supervisor.supervisor(),
          running: boolean(),
          internal_queue: :queue.queue(),
          invoked: invoked,
          subscribed: [{pid(), reference()}]
        }

  @type invoked :: %{invoked_id => invoked_service}
  @type invoked_service :: %{
          id: invoked_id,
          pid: GenServer.server(),
          ref: reference(),
          autoforward: boolean(),
          interpreter_alias: reference()
        }
  @type invoked_id :: String.t()

  @type options :: [option]
  @type option ::
          {:machine, Machine.t()}
          | {:handler, module()}
          | {:parent, GenServer.server()}
          | {:supervisor, Supervisor.supervisor()}

  @type metadata :: %{
          state: %{value: State.value()},
          event: Protean.event()
        }

  # Partial Access behaviour (not defining `pop/2`)
  @doc false
  def fetch(interpreter, key), do: Map.fetch(interpreter, key)
  @doc false
  def get_and_update(interpreter, key, fun), do: Map.get_and_update(interpreter, key, fun)

  @doc """
  Create a new `Interpreter`. The returned interpreter will still need to be started, which could
  result in additional side-effects. See `start/1`.
  """
  @spec new(options) :: Interpreter.t()
  def new(opts) do
    machine = Keyword.fetch!(opts, :machine)

    %Interpreter{
      machine: machine,
      state: Machine.initial_state(machine),
      handler: Keyword.fetch!(opts, :handler),
      parent: Keyword.get(opts, :parent),
      supervisor: Keyword.get(opts, :supervisor)
    }
  end

  @doc "Whether the interpreter has been started and can accept events."
  @spec running?(t) :: boolean()
  def running?(%Interpreter{running: true}), do: true
  def running?(%Interpreter{running: false}), do: false

  @doc """
  Entrypoint for the interpreter that must be called before the interpreter will be in a state
  where it can handle external events. This is necessary in order to handle any initializing
  actions, invokes, or automatic transitions.

  Calling `start/1` on an already-running interpreter is a no-op.
  """
  @spec start(t) :: t
  def start(%Interpreter{running: false} = interpreter) do
    %{interpreter | running: true}
    |> add_internal(Utils.internal_event(:init))
    |> run_interpreter()
  end

  def start(interpreter), do: interpreter

  @doc """
  Stop an interpreter, preventing further event processing and terminating any invoked processes.
  """
  @spec stop(t) :: t
  def stop(%Interpreter{running: false} = interpreter), do: interpreter

  def stop(interpreter) do
    interpreter.invoked
    |> Map.values()
    |> Enum.each(fn %{pid: pid} -> Process.exit(pid, :shutdown) end)

    %{interpreter | running: false, invoked: []}
  end

  @doc """
  Send an event to a running interpreter. This will execute any transitions, actions, and side-
  effects associated with the current machine state and this event.
  """
  @spec send_event(t, Protean.sendable_event()) :: t
  def send_event(%Interpreter{running: true} = interpreter, event) do
    event = Protean.event(event)

    interpreter
    |> autoforward_event(event)
    |> process_event(event)
  end

  def send_event(interpreter, _event), do: interpreter

  def subscribe(interpreter, subscriber) do
    update_in(interpreter.subscribed, &[subscriber | &1])
  end

  def unsubscribe(interpreter, subscriber) do
    update_in(interpreter.subscribed, &Enum.reject(&1, fn s -> s == subscriber end))
  end

  @doc false
  @spec notify_process_down(t, ref: reference()) :: t
  @spec notify_process_down(t, id: invoked_id) :: t
  def notify_process_down(%Interpreter{} = interpreter, ref: ref) do
    invoked = get_invoked_by_ref(interpreter, ref)
    notify_process_down(interpreter, id: invoked[:id])
  end

  def notify_process_down(%Interpreter{} = interpreter, id: id) do
    interpreter
    |> update_in([:invoked], &Map.delete(&1, id))
    |> add_internal(Utils.internal_event(:invoke, :error, id))
    |> run_interpreter()
  end

  defp get_invoked_by_ref(%{invoked: invoked}, ref) do
    invoked
    |> Map.values()
    |> Enum.find(fn %{ref: ^ref} = i -> i end)
  end

  @doc """
  Sets the context of the current state.
  """
  @spec with_context(t, State.context()) :: t
  def with_context(%Interpreter{} = interpreter, context) do
    put_in(interpreter.state.context, context)
  end

  @doc """
  Return the current machine state.
  """
  @spec state(t) :: State.t()
  def state(%Interpreter{state: state}), do: state

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
    interpreter_with_event = set_event(interpreter, event)
    transitions = select_transitions(interpreter_with_event, event)

    transitions
    |> microstep(interpreter_with_event)
    |> run_interpreter()
    |> notify_subscribers()
  end

  defp notify_subscribers(interpreter) do
    Enum.each(interpreter.subscribed, fn {pid, ref} ->
      send(pid, {:state, state(interpreter), ref})
    end)

    interpreter
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

  @spec select_automatic_transitions(t) :: [Transition.t()]
  defp select_automatic_transitions(%{machine: machine, state: state}) do
    Machine.select_automatic_transitions(machine, state)
  end

  @spec select_transitions(t, Protean.event()) :: [Transition.t()]
  defp select_transitions(%{machine: machine, state: state}, event) do
    Machine.select_transitions(machine, state, event)
  end

  @spec autoforward_event(t, Protean.event()) :: t
  defp autoforward_event(%Interpreter{invoked: invoked} = interpreter, event) do
    invoked
    |> Map.values()
    |> Enum.filter(&autoforward?/1)
    |> Enum.reduce(interpreter, &autoforward_to(&1, event, &2))
  end

  @spec autoforward?(invoked_service) :: boolean()
  defp autoforward?(%{autoforward: true}), do: true
  defp autoforward?(_invoke), do: false

  @spec autoforward_to(invoked_service, Protean.event(), t) :: t
  defp autoforward_to(%{pid: pid}, event, interpreter) do
    # TODO: do we need to handle failures here? can add to internal queue
    # if errors occur.
    Interpreter.Server.send_event_async(pid, event)
    interpreter
  end

  @spec select_invokes(t) :: [term()]
  defp select_invokes(_interpreter) do
    # TODO
    []
  end

  @spec invoke(t, [term()]) :: t
  defp invoke(interpreter, _to_invoke) do
    # TODO
    interpreter
  end

  # A microstep fully processes a set of transitions, updating the state configuration and
  # executing any resulting actions.
  defp microstep(transitions, %Interpreter{state: state, machine: machine} = interpreter) do
    {actions, state} =
      machine
      |> Machine.take_transitions(state, transitions)
      |> State.pop_actions()

    final_states =
      state.value
      |> MapSet.difference(interpreter.state.value)
      |> Machine.final_ancestors(machine, state)

    final_states
    |> Enum.map(&Utils.internal_event(:done, &1))
    |> Enum.reduce(interpreter, &add_internal(&2, &1))
    |> with_state(state)
    |> exec_all(actions)
    |> then(&if machine.root.id in final_states, do: stop(&1), else: &1)
  end

  defp exec_all(interpreter, [action | rest]) do
    case Action.exec(action, interpreter) do
      {:halt, interpreter} -> interpreter
      {:cont, interpreter} -> exec_all(interpreter, rest)
      {:cont, interpreter, actions} -> exec_all(interpreter, actions ++ rest)
    end
  end

  defp exec_all(interpreter, []), do: interpreter

  defp add_internal(interpreter, name) when is_binary(name) do
    add_internal(interpreter, {name, nil})
  end

  defp add_internal(interpreter, event) do
    update_in(interpreter.internal_queue, &:queue.in(event, &1))
  end

  defp with_state(interpreter, state) do
    put_in(interpreter.state, state)
  end
end
