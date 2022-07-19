defmodule Protean.Action.Effect do
  @moduledoc """
  Behaviour for arbitrary side-effects that do not affect state.
  """

  alias __MODULE__
  alias Protean.Action
  alias Protean.Action.Protocol.Executable
  alias Protean.Action.Protocol.Resolvable
  alias Protean.State

  @doc "Invoked to handle side-effecting actions."
  @callback effect(Action.name(), State.t(), State.context()) :: any

  defstruct [:action_name, :handler]

  defimpl Resolvable, for: Effect do
    def resolve(effect, _state, handler) do
      %{effect | handler: handler}
    end
  end

  defimpl Executable, for: Effect do
    def exec(%{action_name: action_name, handler: handler}, %{state: state} = interpreter) do
      handler.effect(action_name, state, state.context)
      interpreter
    end
  end
end
