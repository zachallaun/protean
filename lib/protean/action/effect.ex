defmodule Protean.Action.Effect do
  @moduledoc """
  Behaviour for a "fire-and-forget" side-effecting action.
  """

  alias Protean.{Action, Machine, Interpreter}

  @doc "Invoked to handle side-effecting actions."
  @callback effect(Action.name(), Machine.context(), Machine.event(), Interpreter.metadata()) ::
              any

  defstruct [:action_name, :handler, :meta]
end

defimpl Protean.Action.Resolvable, for: Protean.Action.Effect do
  def resolve(effect, context, handler, meta) do
    {%{effect | handler: handler, meta: meta}, context}
  end
end

defimpl Protean.Action.Executable, for: Protean.Action.Effect do
  def exec(effect, context, interpreter) do
    %Protean.Action.Effect{
      action_name: action_name,
      handler: handler,
      meta: meta
    } = effect

    args = [action_name, context, meta.event, meta]
    apply(handler, :effect, args)

    interpreter
  end
end
