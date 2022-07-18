defmodule Protean.Action.Assign do
  @moduledoc "TODO"

  alias __MODULE__
  alias Protean.Action
  alias Protean.State

  defstruct [:merge]

  @type t :: %Assign{
          merge: %{any => any}
        }

  defimpl Action.Protocol.Resolvable, for: Assign do
    def resolve(assign, _, _, _), do: assign
  end

  defimpl Action.Protocol.Executable, for: Assign do
    def exec(%Assign{merge: context}, _context, interpreter) do
      update_in(interpreter.state, &State.assign(&1, context))
    end
  end
end
