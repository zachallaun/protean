defmodule Protean.Action do
  @moduledoc """
  Protean manages state, processes, and side-effects through the execution of _actions_, which
  are data descriptors of things that should happen as a part of the machine lifecycle.

  Protean actions are `{module, arg}` tuples where the module is expected to implement
  `c:exec_action/2` defined by this module. However, Protean defines action creators and
  additional syntax sugar such that fully custom actions should be rare.

  ### Pure & Effect

  The most common "hook" for a Protean machine are the `pure/3` and `effect/3` callbacks, which
  are used to generate actions or produce side-effects at runtime, respectively.

  ### Actions

  See individual action documentation below for details.
  """

  alias Protean.Interpreter
  alias Protean.State
  alias Protean.Transition.Guard
  alias Protean.Utilities, as: Utils

  @typedoc "The string name of an action used to pattern match in a handler."
  @type name :: String.t()

  @typedoc """
  An action is a 2-element tuple, where `handler` implements the `Protean.Action` behaviour and
  `action_arg` is an argument that will be passed back to the handler during execution.
  """
  @type action :: {handler :: module(), action_arg :: any()}

  @type exec_action_return ::
          {:cont, Interpreter.t()}
          | {:cont, Interpreter.t(), [action]}
          | {:halt, Interpreter.t()}

  @doc """
  Accepts the `action_arg` passed with the action as well as the Protean interpreter. Returns
  one of three values:

    * `{:cont, interpreter}` to continue running the action pipeline
    * `{:cont, interpreter, [action]}` to inject actions that will be run immediately before
      running the rest of the pipeline's actions
    * `{:halt, interpreter}` to halt the action pipeline, canceling any further actions
  """
  @callback exec_action(action_arg :: any(), Interpreter.t()) :: exec_action_return

  @doc "Executes an action given an interpreter. See `c:exec_action/2`."
  @spec exec(action, Interpreter.t()) :: exec_action_return
  @spec exec(name, Interpreter.t()) :: exec_action_return
  def exec(action_name, interpreter) when is_binary(action_name) do
    exec({__MODULE__, {:literal, action_name}}, interpreter)
  end

  def exec({handler, arg}, interpreter) do
    case handler.exec_action(arg, interpreter) do
      {:cont, interpreter, []} -> {:cont, interpreter}
      {:cont, interpreter, actions} when is_list(actions) -> {:cont, interpreter, actions}
      {:cont, interpreter} -> {:cont, interpreter}
      {:halt, interpreter} -> {:halt, interpreter}
      other -> raise "Unknown return from #{inspect(handler)}.exec_action/2: #{inspect(other)}"
    end
  end

  @doc "TODO"
  @doc type: :action
  def pure(%State{} = state, action_name), do: pure(action_name) |> put_action(state)

  def pure(action_name) when is_binary(action_name) do
    {__MODULE__, {:pure, action_name}}
  end

  @doc "TODO"
  def effect(%State{} = state, action_name), do: effect(action_name) |> put_action(state)

  def effect(action_name) when is_binary(action_name) do
    {__MODULE__, {:effect, action_name}}
  end

  @doc "TODO"
  def assign(%State{} = state, key, value), do: assign(key, value) |> put_action(state)
  def assign(%State{} = state, assigns), do: assign(assigns) |> put_action(state)

  def assign(key, value) do
    {__MODULE__, {:assign, :merge, %{key => value}}}
  end

  def assign(fun) when is_function(fun) do
    {__MODULE__, {:assign, :update, fun}}
  end

  def assign(assigns) do
    {__MODULE__, {:assign, :merge, Enum.into(assigns, %{})}}
  end

  @doc "TODO"
  def assign_in(%State{} = state, path, value), do: assign_in(path, value) |> put_action(state)

  def assign_in(path, fun) when is_function(fun) do
    assign(fn %{context: context} -> update_in(context, path, fun) end)
  end

  def assign_in(path, value) do
    assign(fn %{context: context} -> put_in(context, path, value) end)
  end

  @doc "TODO"
  def send_event(%State{} = state, event, opts) do
    send_event(event, opts) |> put_action(state)
  end

  def send_event(event, opts \\ []) do
    if delay = opts[:delay] do
      {__MODULE__, {:send_event_after, event, opts[:to], delay}}
    else
      {__MODULE__, {:send_event, event, opts[:to]}}
    end
  end

  @doc "TODO"
  def choose(%State{} = state, actions), do: choose(actions) |> put_action(state)

  def choose(actions) when is_list(actions) do
    {__MODULE__, {:choose, actions}}
  end

  @doc false
  def invoke(:proc, id, proc) do
    {__MODULE__, {:invoke, :proc, id, proc}}
  end

  def invoke(:task, id, task) do
    {__MODULE__, {:invoke, :task, id, task, Utils.internal_event(:invoke, :done, id)}}
  end

  def invoke(:delayed_send, id, delay) do
    task = {:timer, :sleep, [delay]}
    {__MODULE__, {:invoke, :task, id, task, id}}
  end

  def invoke(:cancel, id) do
    {__MODULE__, {:invoke, :cancel, id}}
  end

  # Action callbacks

  def exec_action({:literal, action_name}, interpreter) do
    {:cont, interpreter, [pure(action_name), effect(action_name)]}
  end

  def exec_action({:pure, action_name}, interpreter) do
    %{state: state, handler: handler} = interpreter

    case handler.pure(action_name, state, state.event) do
      nil -> {:cont, interpreter}
      %State{} = state -> {:cont, interpreter, State.actions(state)}
    end
  end

  def exec_action({:effect, action_name}, interpreter) do
    %{state: state, handler: handler} = interpreter
    handler.effect(action_name, state, state.event)
    {:cont, interpreter}
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

  def exec_action({:send_event, event, to}, interpreter) do
    interpreter
    |> resolve_recipient(to)
    |> Protean.send_event_async(event)

    {:cont, interpreter}
  end

  def exec_action({:send_event_after, event, to, delay}, interpreter) do
    interpreter
    |> resolve_recipient(to)
    |> Protean.send_event_after(event, delay)

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
          Protean.DynamicSupervisor.terminate_child(pid)
          Process.demonitor(ref, [:flush])
          update_in(interpreter.invoked, &Map.delete(&1, id))

        _ ->
          interpreter
      end

    {:cont, interpreter}
  end

  def exec_action({:invoke, :proc, id, proc}, interpreter) do
    child_spec_fun = fn pid ->
      # FIXME: This doesn't work if proc is already a tuple
      {proc, [parent: pid]}
    end

    __invoke__(id, child_spec_fun, interpreter)
  end

  def exec_action({:invoke, :task, id, name, event_name}, interpreter)
      when is_binary(name) do
    %{state: state, handler: handler} = interpreter
    task = handler.invoke(name, state, state.event)
    exec_action({:invoke, :task, id, task, event_name}, interpreter)
  end

  def exec_action({:invoke, :task, id, task, event_name}, interpreter) do
    child_spec_fun = fn pid ->
      Task.child_spec(fn -> task |> run_task() |> send_result(pid, event_name) end)
    end

    __invoke__(id, child_spec_fun, interpreter)
  end

  defp __invoke__(id, child_spec_fun, interpreter) do
    self_alias = :erlang.alias()

    interpreter =
      case Protean.DynamicSupervisor.start_child(child_spec_fun.(self_alias)) do
        {:ok, child} ->
          ref = Process.monitor(child)

          update_in(
            interpreter.invoked,
            &Map.put(&1, id, %{
              id: id,
              pid: child,
              ref: ref,
              autoforward: false,
              interpreter_alias: self_alias
            })
          )

        {:error, _} ->
          Interpreter.notify_process_down(interpreter, id: id)
      end

    {:cont, interpreter}
  end

  defp run_task({m, f, a}), do: apply(m, f, a)
  defp run_task(f) when is_function(f), do: f.()

  defp send_result(result, to, event_name) do
    send(to, {event_name, result})
  end

  defp guard_allows?({_, when: guard}, interpreter) do
    %{state: state, handler: handler} = interpreter
    Guard.allows?(guard, state, state.event, handler)
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
end
