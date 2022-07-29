defprotocol Protean.Action.Protocol.Resolvable do
  @moduledoc """
  The `Resolvable` protocol defines `resolve/3` which is used to resolve actions at runtime to
  actions that can be executed (see `Protean.Action.Protocol.Executable`) or additional actions
  that still need to be resolved. These resolved and unresolved actions are all stored in the
  machine state.

  This indirection allows for the creation of "higher-order actions" that produce other actions.
  """

  alias Protean.Action
  alias Protean.State

  @type zero_or_more_resolved :: Action.resolved() | [Action.resolved()] | nil

  @type zero_or_more_unresolved :: Action.unresolved() | [Action.unresolved()] | nil

  @spec resolve(Action.unresolved(), State.t(), module()) ::
          zero_or_more_resolved | {zero_or_more_resolved, zero_or_more_unresolved}
  def resolve(unresolved, state, handler)
end
