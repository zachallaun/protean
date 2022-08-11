defprotocol Protean.Guard do
  @moduledoc """
  Protocol for guarded transitions, actions, etc.

  Default implementations are provided for:

    * `BitString` - Call callback module with string, context, and event
    * `Atom` - Call callback module with atom, context, and event
    * `Function` - Call function with context and event
    * `Tuple` - Higher-order guard utilities:
      * `{:and, [guard1, ...]}`
      * `{:or, [guard1, ...]}`
      * `{:not, guard}`
      * `{:in, query}` - Delegates to `Protean.Context.matches?/2`

  """

  @typedoc "Implements `Protean.Guard` protocol."
  @type guard :: term()

  @spec allows?(t, Protean.Context.t(), Protean.event(), callback_module :: module()) :: boolean()
  def allows?(guard, context, event, module)
end

defimpl Protean.Guard, for: BitString do
  def allows?(name, context, event, module) do
    module.guard(name, context, event)
  end
end

defimpl Protean.Guard, for: Atom do
  def allows?(name, context, event, module) do
    module.guard(name, context, event)
  end
end

defimpl Protean.Guard, for: Function do
  def allows?(guard_fun, context, event, _module) do
    guard_fun.(context, event)
  end
end

defimpl Protean.Guard, for: Tuple do
  def allows?({:and, guards}, context, event, module) when is_list(guards) do
    Enum.all?(guards, &Protean.Guard.allows?(&1, context, event, module))
  end

  def allows?({:or, guards}, context, event, module) when is_list(guards) do
    Enum.any?(guards, &Protean.Guard.allows?(&1, context, event, module))
  end

  def allows?({:not, guard}, context, event, module) do
    !Protean.Guard.allows?(guard, context, event, module)
  end

  def allows?({:in, match_query}, context, _event, _module) do
    Protean.Context.matches?(context, match_query)
  end
end
