defmodule Protean.Interpreter do
  @moduledoc """
  Execution logic for a Protean machine.

  ## Overview of interpretation loop

  At a high level, an SCXML-confirming statechart (which Protean is based on) defines an
  interpreter loop that maintains certain properties laid out in the SCXML specification. In
  addition to executing purely-functional state transformations, the interpreter is responsible
  for executing actions, side-effects, and tracking process state.

  The interpreter loop consists of the following steps:

    1. Check if interpreter is running; if not, exit.
    2. Execute any automatic transitions that are active in the current state, recursively until
       there are no more automatic transitions to be run.
    3. Process internal event if one is present.
      a. Microstep the interpreter by running any transitions and executing any resulting
         actions.
      b. Loop back to 1.
    4. Await an external event.
    5. Process external event in the same manner as 3.

  Note that the loop above contains a conceptually "blocking" operation in awaiting an external
  event. `Protean.Interpreter.start/1` executes the first 3 steps of the interpreter, taking any
  automatic transitions that are immediately active based on the machine configuration, etc. This
  guarantees that the machine ends in a stable state where no more automatic transitions or
  internal events are left, at which point we can wait for an external event.

  > #### Macrosteps and microsteps {: .tip}
  >
  > The execution of this loop until it is waiting for an external event (or interpretation has
  > stopped) is called a _macrostep_. The execution of a single set of transitions resulting from
  > an automatic transition, internal event, or external event is called a _microstep_. Macrosteps
  > can consist of one or more microsteps.

  After this initialization phase, the trigger for the loop to continue is that we've received an
  external event. In the context of Protean, it's easier to think of this as initialization and
  event handling.

  Initialization:

    1. Check if the interpreter is running; if not, exit.
    2. Execute any automatic transitions that are active, looping back to 1. When there are none
       left, continue.
    3. Process internal event if one is present, looping back to 1.

  This guarantees that the machine is in a stable state with no more automatic transitions or
  internal events left to run.

  Handle external event:

    1. Run any transitions and execute actions associated with the external event.
    2. Check if the interpreter is running; if not, exit. (A transition may have caused the
       machine to enter a final state.)
    3. Execute any automatic transitions that have become active as a result of the event,
       looping back to 2.
    4. Process internal event if one is present, looping back to 2.

  As with initialization, this guarantees that the machine is in a stable state, ready to handle
  the next external event.

  The SCXML specification references "run-to-completion", which refers to the property that,
  after interpretation has started, there should be no (visible) point where the interpreter is in
  a non-stable state. When the interpreter starts, it transitions until it is stable. When it
  receives an event, it transitions until it is stable.
  """

  alias Protean.Action
  alias Protean.Context
  alias Protean.Context.Store
  alias Protean.Events
  alias Protean.Interpreter.Features
  alias Protean.Interpreter.Hooks
  alias Protean.MachineConfig
  alias Protean.Machinery

  @doc """
  Initialize an interpreter in the given store.

  The interpreter will still need to be started with `start/1`.
  """
  @spec initialize(keyword()) :: Store.t()
  def initialize(opts) do
    store = Keyword.fetch!(opts, :store)
    config = Keyword.fetch!(opts, :machine)

    ctx =
      config
      |> MachineConfig.initial_context()
      |> Map.put(:id, Keyword.get(opts, :id))
      |> Map.put(:parent, Keyword.get(opts, :parent))
      |> Features.install()

    Store.put_context(store, ctx)
  end

  @doc "Whether the interpreter has been started and can accept events."
  @spec running?(Store.t()) :: boolean()
  def running?(store), do: Context.get(store, :running)

  @doc """
  Entrypoint for the interpreter that must be called before the interpreter will be in a state
  where it can handle external events. This is necessary in order to handle any initializing
  actions, spawns, or automatic transitions.

  Calling `start/1` on an already-running interpreter is a no-op.
  """
  @spec start(Store.t()) :: Store.t()
  def start(store) do
    unless running?(store) do
      store
      |> Context.put(:running, true)
      |> Context.add_internal(Events.platform(:init))
      |> run_interpreter()
    else
      store
    end
  end

  @doc """
  Stop an interpreter, preventing further event processing.
  """
  @spec stop(Store.t()) :: Store.t()
  def stop(store) do
    if running?(store) do
      store
      |> Context.put(:running, false)
      |> Hooks.run(:on_stop)
    else
      store
    end
  end

  @doc """
  Handle an event, executing any transitions, actions, and side-effects associated with the
  current machine context.

  Returns a tuple of the interpreter and any replies resulting from actions that were run.
  """
  @spec handle_event(Store.t(), Protean.event()) :: {Store.t(), [term()]}
  def handle_event(store, event) do
    if running?(store) do
      store
      |> macrostep(event)
      |> Context.pop_replies()
    else
      {store, []}
    end
  end

  # Entrypoint for the SCXML main event loop. Ensures that any automatic transitions are run and
  # internal events are processed before processing any external events.
  @spec run_interpreter(Store.t()) :: Store.t()
  defp run_interpreter(store) do
    if running?(store) do
      store
      |> run_automatic_transitions()
      |> process_internal_queue()
    else
      store
    end
  end

  defp run_automatic_transitions(store) do
    case select_automatic_transitions(store) do
      [] ->
        store

      transitions ->
        transitions
        |> microstep(store)
        |> run_automatic_transitions()
    end
  end

  defp process_internal_queue(store) do
    queue = Context.get(store, :internal_queue)

    case :queue.out(queue) do
      {:empty, _} ->
        store

      {{:value, event}, queue} ->
        store
        |> Context.put(:internal_queue, queue)
        |> macrostep(event)
    end
  end

  defp macrostep(store, event) do
    store
    |> process_event(event)
    |> Hooks.run(:after_macrostep)
  end

  defp microstep(transitions, store) do
    ctx = Context.Store.get_context(store)

    {actions, next_ctx} =
      ctx
      |> Machinery.take_transitions(transitions)
      |> Context.pop_actions()

    final_state_events =
      next_ctx.final
      |> MapSet.difference(ctx.final)
      |> Enum.map(&Events.platform(:done, &1))

    store
    |> Context.Store.put_context(next_ctx)
    |> Context.add_many_internal(final_state_events)
    |> Action.exec_all(actions)
    |> stop_if_final()
    |> Hooks.run(:after_microstep)
  end

  defp stop_if_final(store) do
    config = Context.get(store, :config)

    if config.root.id in Context.get(store, :final) do
      stop(store)
    else
      store
    end
  end

  defp process_event(store, event) do
    with {store, event} <- Hooks.run(store, :event_filter, event),
         store <- Hooks.run(store, :after_event_filter, event) do
      store
      |> select_transitions(event)
      |> microstep(set_event(store, event))
      |> run_interpreter()
    else
      store ->
        store
        |> run_interpreter()
    end
  end

  defp set_event(store, %Events.Platform{payload: nil}), do: store

  defp set_event(store, %Events.Platform{payload: payload}) do
    Context.put(store, :event, payload)
  end

  defp set_event(store, event) do
    Context.put(store, :event, event)
  end

  defp select_automatic_transitions(store) do
    store
    |> Context.Store.get_context()
    |> Machinery.select_transitions(:automatic_transitions)
  end

  defp select_transitions(store, event) do
    store
    |> Context.Store.get_context()
    |> Machinery.select_transitions(event)
  end
end
