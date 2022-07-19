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

  defmodule Resolved do
    @moduledoc false

    defstruct [:action_name, :handler]

    defimpl Executable, for: __MODULE__ do
      def exec(%{action_name: action_name, handler: handler}, %{state: state} = interpreter) do
        handler.effect(action_name, state, state.context)
        interpreter
      end
    end
  end

  defmodule Unresolved do
    @moduledoc false

    defstruct [:action_name]

    defimpl Resolvable, for: __MODULE__ do
      def resolve(%{action_name: action_name}, _state, handler),
        do: %Effect.Resolved{action_name: action_name, handler: handler}
    end
  end
end
