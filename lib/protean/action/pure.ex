defmodule Protean.Action.Pure do
  @moduledoc """
  Behaviour for an action without side-effects that can update machine context and create
  additional actions.
  """

  alias __MODULE__
  alias Protean.Action
  alias Protean.Action.Protocol.Resolvable
  alias Protean.State

  @doc "Invoked to handle pure actions."
  @callback pure(Action.name(), State.t(), State.context()) :: State.t() | nil

  defstruct [:action_name]

  defimpl Resolvable, for: Pure do
    def resolve(%{action_name: action_name}, state, handler) do
      with %State{} = state <- handler.pure(action_name, state, state.context) do
        {nil, State.actions(state)}
      end
    end
  end
end
