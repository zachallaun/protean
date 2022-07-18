defprotocol Protean.Action.Protocol.Resolvable do
  @moduledoc """
  The `Resolvable` protocol defines `resolve/4`, which resolves an unresolved
  action without side-effects, returning the resolved action, the context
  that action should be bound to, and any additional unresolved actions that
  still need to be resolved.
  """

  alias Protean.Action
  alias Protean.Interpreter
  alias Protean.Machine

  @type resolved_return ::
          {Action.resolved() | nil, Machine.context(), [Action.unresolved()]}
          | {Action.resolved() | nil, Machine.context()}
          | Action.resolved()

  @spec resolve(Action.unresolved(), Machine.context(), module, Interpreter.metadata()) ::
          resolved_return
  def resolve(unresolved, context, handler, meta)
end
