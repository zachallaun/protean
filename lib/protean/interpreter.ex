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

  alias __MODULE__
  alias Protean.Action
  alias Protean.Context
  alias Protean.Events
  alias Protean.Interpreter.Features
  alias Protean.Interpreter.Hooks
  alias Protean.MachineConfig
  alias Protean.Machinery
  alias Protean.Transition

  defstruct [
    :id,
    :config,
    :context,
    :parent,
    running: false,
    internal_queue: :queue.new(),
    hooks: %{}
  ]

  @type t :: %Interpreter{
          id: Protean.id() | nil,
          config: MachineConfig.t(),
          context: Context.t(),
          parent: pid(),
          running: boolean(),
          internal_queue: :queue.queue(),
          hooks: map()
        }

  @doc """
  Create a new `Interpreter`. The returned interpreter will still need to be started, which could
  result in additional side-effects. See `start/1`.
  """
  @spec new(keyword()) :: Interpreter.t()
  def new(opts) do
    config = Keyword.fetch!(opts, :machine)
    initial_assigns = Keyword.get(opts, :assigns, %{})

    context =
      config
      |> MachineConfig.initial_context()
      |> Context.assign(initial_assigns)

    %Interpreter{
      id: Keyword.get(opts, :id),
      config: config,
      context: context,
      parent: Keyword.get(opts, :parent)
    }
    |> Features.install()
  end

  @doc false
  @spec add_internal(t, term()) :: t
  def add_internal(interpreter, event) do
    update_in(interpreter.internal_queue, &:queue.in(event, &1))
  end

  @doc false
  @spec with_context(t, map()) :: t
  def with_context(interpreter, context) do
    put_in(interpreter.context, context)
  end

  @doc false
  @spec put_reply(t, term()) :: t
  def put_reply(interpreter, reply) do
    update_in(interpreter.context, &Context.put_reply(&1, reply))
  end

  @doc "Whether the interpreter has been started and can accept events."
  @spec running?(t) :: boolean()
  def running?(%Interpreter{running: true}), do: true
  def running?(%Interpreter{running: false}), do: false

  @doc """
  Entrypoint for the interpreter that must be called before the interpreter will be in a state
  where it can handle external events. This is necessary in order to handle any initializing
  actions, spawns, or automatic transitions.

  Calling `start/1` on an already-running interpreter is a no-op.
  """
  @spec start(t) :: t
  def start(%Interpreter{running: false} = interpreter) do
    %{interpreter | running: true}
    |> add_internal(Events.platform(:init))
    |> run_interpreter()
  end

  def start(interpreter), do: interpreter

  @doc """
  Stop an interpreter, preventing further event processing.
  """
  @spec stop(t) :: t
  def stop(%Interpreter{running: true} = interpreter) do
    %{interpreter | running: false}
    |> Hooks.run(:on_stop)
  end

  def stop(interpreter), do: interpreter

  @doc """
  Handle an event, executing any transitions, actions, and side-effects associated with the
  current machine context.

  Returns a tuple of the interpreter and any replies resulting from actions that were run.
  """
  @spec handle_event(t, Protean.event()) :: {t, [term()]}
  def handle_event(%Interpreter{running: true} = interpreter, event) do
    interpreter
    |> macrostep(event)
    |> pop_replies()
  end

  def handle_event(interpreter, _event), do: {interpreter, []}

  @spec pop_replies(t) :: {t, [term()]}
  defp pop_replies(%Interpreter{context: context} = interpreter) do
    {replies, context} = Context.pop_replies(context)
    {with_context(interpreter, context), replies}
  end

  @doc """
  Return the current machine context.
  """
  @spec context(t) :: Context.t()
  def context(%Interpreter{context: context}), do: context

  # Entrypoint for the SCXML main event loop. Ensures that any automatic transitions are run and
  # internal events are processed before processing any external events.
  @spec run_interpreter(t) :: t
  defp run_interpreter(%Interpreter{running: true} = interpreter) do
    interpreter
    |> run_automatic_transitions()
    |> process_internal_queue()
  end

  defp run_interpreter(%Interpreter{running: false} = interpreter),
    do: interpreter

  defp run_automatic_transitions(interpreter) do
    case select_automatic_transitions(interpreter) do
      [] ->
        interpreter

      transitions ->
        transitions
        |> microstep(interpreter)
        |> run_automatic_transitions()
    end
  end

  defp process_internal_queue(%Interpreter{internal_queue: queue} = interpreter) do
    case :queue.out(queue) do
      {:empty, _} ->
        interpreter

      {{:value, event}, queue} ->
        %{interpreter | internal_queue: queue}
        |> macrostep(event)
    end
  end

  defp macrostep(interpreter, event) do
    interpreter
    |> process_event(event)
    |> Hooks.run(:after_macrostep)
  end

  defp microstep(transitions, %Interpreter{context: context, config: config} = interpreter) do
    {actions, context} =
      config
      |> Machinery.take_transitions(context, transitions)
      |> Context.pop_actions()

    newly_final =
      context.final
      |> MapSet.difference(interpreter.context.final)

    newly_final
    |> Enum.map(&Events.platform(:done, &1))
    |> Enum.reduce(interpreter, &add_internal(&2, &1))
    |> with_context(context)
    |> Action.exec_all(actions)
    |> then(&if config.root.id in context.final, do: stop(&1), else: &1)
    |> Hooks.run(:after_microstep)
  end

  defp process_event(interpreter, event) do
    with {interpreter, event} <- Hooks.run(interpreter, :event_filter, event),
         interpreter <- Hooks.run(interpreter, :after_event_filter, event) do
      interpreter
      |> select_transitions(event)
      |> microstep(set_event(interpreter, event))
      |> run_interpreter()
    else
      interpreter ->
        interpreter
        |> run_interpreter()
    end
  end

  defp set_event(interpreter, %Events.Platform{payload: nil}), do: interpreter

  defp set_event(interpreter, %Events.Platform{payload: payload}) do
    put_in(interpreter.context.event, payload)
  end

  defp set_event(interpreter, event) do
    put_in(interpreter.context.event, event)
  end

  @spec select_automatic_transitions(t) :: [Transition.t()]
  defp select_automatic_transitions(%{config: machine, context: context}) do
    Machinery.select_transitions(machine, context, context.event, :automatic_transitions)
  end

  @spec select_transitions(t, Protean.event()) :: [Transition.t()]
  defp select_transitions(%{config: machine, context: context}, event) do
    Machinery.select_transitions(machine, context, event)
  end
end
