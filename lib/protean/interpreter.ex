defmodule Protean.Interpreter do
  @moduledoc """
  Execution logic for a Protean machine.
  """

  alias __MODULE__
  alias Protean.Action
  alias Protean.Context
  alias Protean.Events
  alias Protean.MachineConfig
  alias Protean.Machinery
  alias Protean.ProcessManager
  alias Protean.PubSub
  alias Protean.Transition

  defstruct [
    :id,
    :config,
    :context,
    :parent,
    running: false,
    internal_queue: :queue.new()
  ]

  @type t :: %Interpreter{
          id: Protean.id() | nil,
          config: MachineConfig.t(),
          context: Context.t(),
          parent: pid(),
          running: boolean(),
          internal_queue: :queue.queue()
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
  actions, invokes, or automatic transitions.

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
  Stop an interpreter, preventing further event processing and terminating any invoked processes.
  """
  @spec stop(t) :: t
  def stop(%Interpreter{running: false} = interpreter), do: interpreter

  def stop(interpreter) do
    ProcessManager.stop_all_subprocesses()

    %{interpreter | running: false}
  end

  @doc """
  Handle an event, executing any transitions, actions, and side-effects associated with the
  current machine context.

  Returns a tuple of the interpreter and any replies resulting from actions that were run.
  """
  @spec handle_event(t, Protean.event()) :: {t, [term()]}
  def handle_event(%Interpreter{running: true} = interpreter, %Events.Platform{} = event) do
    interpreter
    |> process_event(event, false)
    |> pop_replies()
  end

  def handle_event(%Interpreter{running: true} = interpreter, event) do
    interpreter
    |> autoforward_event(event)
    |> process_event(event)
    |> pop_replies()
  end

  def handle_event(interpreter, _event), do: {interpreter, []}

  @spec pop_replies(t) :: {t, [term()]}
  defp pop_replies(%Interpreter{context: context} = interpreter) do
    {replies, context} = Context.pop_replies(context)
    {with_context(interpreter, context), replies}
  end

  @doc false
  @spec notify_process_down(t, reason :: term(), keyword()) :: t
  def notify_process_down(%Interpreter{} = interpreter, reason, ref: ref) do
    case ProcessManager.subprocess_by_ref(ref) do
      {:ok, {id, _, _, _}} -> notify_process_down(interpreter, reason, id: id)
      nil -> interpreter
    end
  end

  def notify_process_down(%Interpreter{} = interpreter, reason, id: id) do
    if invoke_error?(reason) do
      add_internal(interpreter, Events.platform(:invoke, :error, id))
    else
      interpreter
    end
    |> run_interpreter()
  end

  defp invoke_error?(:normal), do: false
  defp invoke_error?(:shutdown), do: false
  defp invoke_error?({:shutdown, _}), do: false
  defp invoke_error?(_other), do: true

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
  end

  defp run_interpreter(%Interpreter{running: false} = interpreter),
    do: interpreter

  defp run_automatic_transitions(interpreter) do
    case select_automatic_transitions(interpreter) do
      [] ->
        process_internal_queue(interpreter)

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
        |> process_event(event, false)
    end
  end

  defp process_event(interpreter, event, external? \\ true) do
    interpreter_with_event = set_event(interpreter, event)
    transitions = select_transitions(interpreter_with_event, event)

    transitions
    |> microstep(if external?, do: interpreter_with_event, else: interpreter)
    |> run_interpreter()
    |> broadcast_transition()
  end

  defp broadcast_transition(%Interpreter{id: nil} = interpreter) do
    interpreter
  end

  defp broadcast_transition(%Interpreter{id: id, context: context} = interpreter) do
    replies = Context.get_replies(context)
    message = {id, context, replies}

    if Enum.empty?(replies) do
      PubSub.broadcast(id, message, nil)
    else
      PubSub.broadcast(id, message, :replies)
    end
    |> case do
      :ok ->
        :ok

      {:error, error} ->
        require Logger
        Logger.warn("PubSub broadcast error: #{inspect(error)}")
    end

    interpreter
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

  @spec autoforward_event(t, Protean.event()) :: t
  defp autoforward_event(interpreter, event) do
    for {_id, pid, _ref, opts} <- ProcessManager.subprocesses(),
        Keyword.get(opts, :autoforward, false) do
      Interpreter.Server.send(pid, event)
    end

    interpreter
  end

  # A microstep fully processes a set of transitions, updating the state configuration and
  # executing any resulting actions.
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
  end
end
