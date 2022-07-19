defmodule Protean.Action.Assign do
  @moduledoc "TODO"

  alias __MODULE__
  alias Protean.Action.Protocol.Executable
  alias Protean.Action.Protocol.Resolvable
  alias Protean.State

  defstruct [:merge]

  @type t :: %Assign{
          merge: %{any => any}
        }

  defimpl Resolvable, for: Assign do
    def resolve(assign, _state, _handler), do: assign
  end

  defimpl Executable, for: Assign do
    def exec(%Assign{merge: context}, interpreter) do
      update_in(interpreter.state, &State.assign(&1, context))
    end
  end
end
