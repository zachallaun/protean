defmodule Protean.Transition.Guard do
  @moduledoc """
  Defines behaviour for guarded transition handler modules.
  """

  alias Protean.State

  @typedoc "Any type that implements the `Protean.Transition.Guard.Guards` protocol."
  @type guard :: term

  defprotocol Guards do
    @spec allows?(t, State.t(), Protean.event(), module) :: boolean
    def allows?(guard, state, event, handler)
  end

  defimpl Guards, for: BitString do
    def allows?(name, state, event, handler) do
      handler.condition(name, state, event)
    end
  end

  defimpl Guards, for: Function do
    def allows?(guard_fun, state, event, _handler) do
      guard_fun.(state, event)
    end
  end

  defimpl Guards, for: Tuple do
    def allows?({:and, guards}, state, event, handler) when is_list(guards) do
      Enum.all?(guards, &Guards.allows?(&1, state, event, handler))
    end

    def allows?({:or, guards}, state, event, handler) when is_list(guards) do
      Enum.any?(guards, &Guards.allows?(&1, state, event, handler))
    end

    def allows?({:not, guard}, state, event, handler) do
      !Guards.allows?(guard, state, event, handler)
    end

    def allows?({:in, match_query}, state, _event, _handler) do
      State.matches?(state, match_query)
    end
  end

  defdelegate allows?(guard, state, event, handler), to: Guards
end
