defmodule Protean.Action.Pure do
  @moduledoc """
  Behaviour for an action without side-effects that can optionally update
  machine context or create additional actions.
  """

  alias __MODULE__
  alias Protean.Action
  alias Protean.Interpreter
  alias Protean.Machine

  @doc "Invoked to handle pure actions."
  @callback pure(Action.name(), Machine.context(), Machine.event(), Interpreter.metadata()) ::
              nil
              | Machine.context()
              | {Machine.context(), [Action.unresolved()]}

  defstruct [:action_name]

  defimpl Action.Protocol.Resolvable, for: Pure do
    def resolve(%{action_name: action_name}, context, handler, meta) do
      args = [action_name, context, meta.event, meta]

      apply(handler, :pure, args)
      |> normalize(context)
    end

    defp normalize(nil, context), do: {nil, context}
    defp normalize(%{} = context, _), do: {Action.assign(context), context}
    defp normalize({context, nil}, _), do: {Action.assign(context), context}

    defp normalize({context, actions}, _),
      do: {Action.assign(context), context, List.wrap(actions)}
  end
end
