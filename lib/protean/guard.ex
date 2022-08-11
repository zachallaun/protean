defprotocol Protean.Guard do
  @moduledoc """
  Protocol for guarded transitions, actions, etc.

  Default implementations are provided for:

    * `BitString` - Call callback module with string, state, and event
    * `Atom` - Call callback module with atom, state, and event
    * `Function` - Call function with state and event
    * `Tuple` - Higher-order guard utilities:
      * `{:and, [guard1, ...]}`
      * `{:or, [guard1, ...]}`
      * `{:not, guard}`
      * `{:in, query}` - Delegates to `Protean.Context.matches?/2`

  """

  @typedoc "Implements `Protean.Guard` protocol."
  @type guard :: term()

  @spec allows?(t, Protean.Context.t(), Protean.event(), callback_module :: module()) :: boolean()
  def allows?(guard, state, event, module)
end

defimpl Protean.Guard, for: BitString do
  def allows?(name, state, event, module) do
    module.guard(name, state, event)
  end
end

defimpl Protean.Guard, for: Atom do
  def allows?(name, state, event, module) do
    module.guard(name, state, event)
  end
end

defimpl Protean.Guard, for: Function do
  def allows?(guard_fun, state, event, _module) do
    guard_fun.(state, event)
  end
end

defimpl Protean.Guard, for: Tuple do
  def allows?({:and, guards}, state, event, module) when is_list(guards) do
    Enum.all?(guards, &Protean.Guard.allows?(&1, state, event, module))
  end

  def allows?({:or, guards}, state, event, module) when is_list(guards) do
    Enum.any?(guards, &Protean.Guard.allows?(&1, state, event, module))
  end

  def allows?({:not, guard}, state, event, module) do
    !Protean.Guard.allows?(guard, state, event, module)
  end

  def allows?({:in, match_query}, state, _event, _module) do
    Protean.Context.matches?(state, match_query)
  end
end
