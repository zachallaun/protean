defmodule Protean.Interpreter do
  @moduledoc """
  Execution logic for a Protean machine.
  """

  alias __MODULE__
  alias Protean.Action
  alias Protean.Events
  alias Protean.MachineConfig
  alias Protean.Machinery
  alias Protean.Context

  defstruct [
    :config,
    :context,
    :parent,
    :supervisor,
    running: false,
    internal_queue: :queue.new(),
    invoked: %{},
    subscribers: %{}
  ]

  @type t :: %Interpreter{
          config: MachineConfig.t(),
          context: Context.t(),
          parent: pid(),
          supervisor: Supervisor.supervisor(),
          running: boolean(),
          internal_queue: :queue.queue(),
          invoked: invoked,
          subscribers: %{reference() => %{pid: pid(), to: :all | :replies}}
        }

  @type invoked :: %{invoked_id => invoked_service}
  @type invoked_id :: String.t()
  @opaque invoked_service :: %{
            id: invoked_id,
            pid: GenServer.server(),
            ref: reference(),
            autoforward: boolean(),
            interpreter_alias: reference()
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
  @spec new([Protean.machine_option()]) :: Interpreter.t()
  def new(opts) do
    config = Keyword.fetch!(opts, :machine)
    context = MachineConfig.initial_context(config)
    initial_assigns = Keyword.get(opts, :assigns, %{})

    %Interpreter{
      config: config,
      context: Context.assign(context, initial_assigns),
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
    interpreter.invoked
    |> Map.values()
    |> Enum.each(fn %{pid: pid} -> Process.exit(pid, :shutdown) end)

    %{interpreter | running: false, invoked: []}
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

  defp pop_replies(%Interpreter{context: context} = interpreter) do
    {replies, context} = Context.pop_replies(context)
    {with_context(interpreter, context), replies}
  end

  def subscribe(interpreter, %{pid: pid, ref: ref, to: to}) do
    put_in(interpreter.subscribers[ref], %{pid: pid, to: to})
  end

  def unsubscribe(interpreter, ref) do
    update_in(interpreter.subscribers, &Map.delete(&1, ref))
  end

  @doc false
  @spec notify_process_down(t, reason :: term(), ref: reference()) :: t
  @spec notify_process_down(t, reason :: term(), id: invoked_id) :: t
  def notify_process_down(%Interpreter{} = interpreter, reason, ref: ref) do
    invoked = get_invoked_by_ref(interpreter, ref)
    notify_process_down(interpreter, reason, id: invoked[:id])
  end

  def notify_process_down(%Interpreter{} = interpreter, reason, id: id) do
    interpreter =
      if invoke_error?(reason) do
        add_internal(interpreter, Events.platform(:invoke, :error, id))
      else
        interpreter
      end

    interpreter
    |> update_in([:invoked], &Map.delete(&1, id))
    |> run_interpreter()
  end

  defp invoke_error?(:normal), do: false
  defp invoke_error?(:shutdown), do: false
  defp invoke_error?({:shutdown, _}), do: false
  defp invoke_error?(_other), do: true

  defp get_invoked_by_ref(%{invoked: invoked}, ref) do
    invoked
    |> Map.values()
    |> Enum.find(fn proc -> proc[:ref] === ref end)
  end

  @doc """
  Return the current machine context.
  """
  @spec context(t) :: Context.t()
  def context(%Interpreter{context: context}), do: context

  # Entrypoint for the SCXML main event loop. Ensures that any automatic transitions are run and
  # internal events are processed before awaiting an external event.
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
    |> notify_subscribers()
  end

  defp notify_subscribers(interpreter) do
    context = context(interpreter)
    replies = Context.get_replies(context)

    interpreter.subscribers
    |> Enum.filter(fn {_ref, subscriber} ->
      should_notify?(subscriber, !Enum.empty?(replies))
    end)
    |> Enum.each(fn {ref, %{pid: pid}} ->
      send(pid, {:state, ref, {context, replies}})
    end)

    interpreter
  end

  defp should_notify?(subscriber, has_replies?)
  defp should_notify?(%{to: :replies}, false), do: false
  defp should_notify?(_, _), do: true

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
    Interpreter.Server.send(pid, event)
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
    |> exec_all(actions)
    |> then(&if config.root.id in context.final, do: stop(&1), else: &1)
  end

  defp exec_all(interpreter, [action | rest]) do
    case Action.exec(action, interpreter) do
      {:halt, interpreter} -> interpreter
      {:cont, interpreter} -> exec_all(interpreter, rest)
      {:cont, interpreter, actions} -> exec_all(interpreter, actions ++ rest)
    end
  end

  defp exec_all(interpreter, []), do: interpreter

  @doc false
  def add_internal(interpreter, event) do
    update_in(interpreter.internal_queue, &:queue.in(event, &1))
  end

  @doc false
  def with_context(interpreter, context) do
    put_in(interpreter.context, context)
  end

  @doc false
  def put_reply(interpreter, reply) do
    update_in(interpreter.context, &Context.put_reply(&1, reply))
  end
end
