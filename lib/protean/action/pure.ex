defmodule Protean.Action.Pure do
  @moduledoc """
  Behaviour for an action without side-effects that can optionally update
  machine context or create additional actions.
  """

  alias Protean.{Action, Machine, Interpreter}

  @doc "Invoked to handle pure actions."
  @callback pure(Action.name(), Machine.context(), Machine.event(), Interpreter.metadata()) ::
              nil
              | Machine.context()
              | {Machine.context(), [Action.unresolved()]}

  defstruct [:action_name]
end

defimpl Protean.Action.Resolvable, for: Protean.Action.Pure do
  def resolve(%{action_name: action_name}, context, handler, meta) do
    args = [action_name, context, meta.event, meta]

    apply(handler, :pure, args)
    |> normalize(context)
  end

  defp normalize(nil, context), do: {nil, context}
  defp normalize(%{} = context, _), do: {nil, context}
  defp normalize({context, nil}, _), do: {nil, context}
  defp normalize({context, actions}, _), do: {nil, context, List.wrap(actions)}
end
