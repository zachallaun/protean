defmodule Protean.Action do
  @moduledoc """
  TODO
  """

  alias __MODULE__
  alias Protean.{Machine, Interpreter, Action.Resolvable}

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

  @doc """
  Resolves actions to `t:bound_resolved`, which are `{resolved_action, context}`
  pairs that can be executed later by an interpreter.
  """
  @spec resolve_actions([unresolved], Machine.context(), Module.t(), Interpreter.metadata()) ::
          [bound_resolved]
  def resolve_actions(actions, context, handler, meta) do
    {context, List.wrap(actions)}
    |> Stream.unfold(&resolve_action(&1, handler, meta))
    |> Enum.filter(&elem(&1, 0))
  end

  def resolve_action({context, [action | rest]}, handler, meta) do
    case Resolvable.resolve(action, context, handler, meta) do
      {resolved, context, unresolved} ->
        {{resolved, context}, {context, List.wrap(unresolved) ++ rest}}

      {resolved, context} ->
        {{resolved, context}, {context, rest}}
    end
  end

  def resolve_action({_context, []}, _handler, _meta), do: nil

  defimpl Resolvable, for: BitString do
    def resolve(action_name, context, _handler, _meta) do
      pure = %Action.Pure{action_name: action_name}
      effect = %Action.Effect{action_name: action_name}

      {nil, context, [pure, effect]}
    end
  end
end
