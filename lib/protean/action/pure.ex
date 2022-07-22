defmodule Protean.Action.Pure do
  @moduledoc """
  Behaviour for an action without side-effects that can update machine context and create
  additional actions.
  """

  alias Protean.Action
  alias Protean.Action.Protocol.Resolvable
  alias Protean.Machine
  alias Protean.State

  @doc "Invoked to handle pure actions."
  @callback pure(Action.name(), State.t(), Machine.event()) :: State.t() | nil

  defmodule Unresolved do
    @moduledoc false

    defstruct [:action_name]

    defimpl Resolvable, for: __MODULE__ do
      def resolve(%{action_name: action_name}, state, handler) do
        with %State{} = state <- handler.pure(action_name, state, state.event) do
          {nil, State.actions(state)}
        end
      end
    end
  end
end
