defmodule Protean.Action do
  @moduledoc """
  Protean manages state, processes, and side-effects through the execution of _actions_, which
  are data descriptors of things that should happen in response to state machine transitions.

  ### Pure and Effect
  TODO explain pure/4 and effect/4 callbacks

  ### Resolution and Execution
  TODO explain protocols
  """

  alias __MODULE__
  alias Protean.{Machine, Interpreter}

  @typedoc """
  A resolved action is anything that implements the `Executable` protocol.
  """
  @type resolved :: any

  @typedoc """
  A resolved action and the machine context that it should be executed with.
  """
  @type bound_resolved :: {resolved, Machine.context()}

  @typedoc """
  An unresolved action is anything that implements the `Resolvable` protocol,
  which is used by an interpreter to resolve to `t:resolved()`.
  """
  @type unresolved :: any

  @typedoc "The string name of an action used to pattern match in a handler."
  @type name :: String.t()

  @doc "TODO"
  def pure(action_name) when is_binary(action_name) do
    %Action.Pure{action_name: action_name}
  end

  @doc "TODO"
  def effect(action_name) when is_binary(action_name) do
    %Action.Effect{action_name: action_name}
  end

  @doc "TODO"
  def assign(assigns) when is_map(assigns) do
    %Action.Assign{merge: assigns}
  end

  def assign(assigns) when is_list(assigns) do
    assigns
    |> Enum.into(%{})
    |> assign()
  end

  @doc "TODO"
  def send_event(event, opts \\ []) do
    %Action.SendEvent{event: event, to: opts[:to], delay: opts[:delay]}
  end

  @doc """
  Resolves actions to `t:bound_resolved`, which are `{resolved_action, context}`
  pairs that can be executed later by an interpreter.
  """
  @spec resolve_actions([unresolved], Machine.context(), module, Interpreter.metadata()) ::
          [bound_resolved]
  def resolve_actions(actions, context, handler, meta) do
    {context, List.wrap(actions)}
    |> Stream.unfold(&resolve_action(&1, handler, meta))
    |> Enum.filter(&elem(&1, 0))
  end

  def resolve_action({context, [action | rest]}, handler, meta) do
    case Action.Protocol.Resolvable.resolve(action, context, handler, meta) do
      {resolved, context, unresolved} ->
        {{resolved, context}, {context, List.wrap(unresolved) ++ rest}}

      {resolved, context} ->
        {{resolved, context}, {context, rest}}

      resolved ->
        {{resolved, context}, {context, rest}}
    end
  end

  def resolve_action({_context, []}, _handler, _meta), do: nil

  defimpl Action.Protocol.Resolvable, for: BitString do
    def resolve(action_name, context, _handler, _meta) do
      {nil, context, [Action.pure(action_name), Action.effect(action_name)]}
    end
  end
end
