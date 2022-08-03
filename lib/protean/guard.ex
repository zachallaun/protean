defprotocol Protean.Guard do
  @moduledoc """
  Protocol for guarded transitions, actions, etc.

  Default implementations are provided for:

    * `BitString` - Call handler module with string, state, and event
    * `Function` - Call function with state and event
    * `Tuple` - Higher-order guard utilities:
      * `{:and, [guard1, ...]}`
      * `{:or, [guard1, ...]}`
      * `{:not, guard}`
      * `{:in, query}` - Delegates to `Protean.State.matches?/2`
  """

  @typedoc "Implements `Protean.Guard` protocol."
  @type guard :: term()

  @spec allows?(t, Protean.State.t(), Protean.event(), module()) :: boolean()
  def allows?(guard, state, event, handler)
end

defimpl Protean.Guard, for: BitString do
  def allows?(name, state, event, handler) do
    handler.guard(name, state, event)
  end
end

defimpl Protean.Guard, for: Function do
  def allows?(guard_fun, state, event, _handler) do
    guard_fun.(state, event)
  end
end

defimpl Protean.Guard, for: Tuple do
  def allows?({:and, guards}, state, event, handler) when is_list(guards) do
    Enum.all?(guards, &Protean.Guard.allows?(&1, state, event, handler))
  end

  def allows?({:or, guards}, state, event, handler) when is_list(guards) do
    Enum.any?(guards, &Protean.Guard.allows?(&1, state, event, handler))
  end

  def allows?({:not, guard}, state, event, handler) do
    !Protean.Guard.allows?(guard, state, event, handler)
  end

  def allows?({:in, match_query}, state, _event, _handler) do
    Protean.State.matches?(state, match_query)
  end
end
