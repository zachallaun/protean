defmodule Protean.Transition.Guard do
  @moduledoc """
  Defines behaviour for guarded transition handler modules.
  """

  alias Protean.Machine
  alias Protean.State

  @doc """
  Determines whether a transition should take place given the current machien state.
  """
  @callback condition(
              guard_name :: String.t(),
              context :: Machine.context(),
              event :: Machine.event(),
              metadata :: %{any => any}
            ) :: boolean

  defprotocol Guards do
    @spec allows?(t, Machine.event(), State.t(), module) :: boolean
    def allows?(guard, event, state, handler)
  end

  defimpl Guards, for: BitString do
    def allows?(name, event, state, handler) do
      apply(handler, :condition, [name, state.context, event, %{state: state}])
    end
  end

  defimpl Guards, for: Function do
    def allows?(guard_fun, event, state, _handler) do
      guard_fun.(state.context, event, %{state: state})
    end
  end

  defimpl Guards, for: Tuple do
    def allows?({:and, guards}, event, state, handler) when is_list(guards) do
      Enum.all?(guards, &Guards.allows?(&1, event, state, handler))
    end

    def allows?({:or, guards}, event, state, handler) when is_list(guards) do
      Enum.any?(guards, &Guards.allows?(&1, event, state, handler))
    end

    def allows?({:not, guard}, event, state, handler) do
      !Guards.allows?(guard, event, state, handler)
    end

    def allows?({:in, match_query}, _event, state, _handler) do
      State.matches?(state, match_query)
    end
  end

  defdelegate allows?(guard, event, state, handler), to: Guards
end
