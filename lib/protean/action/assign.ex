defmodule Protean.Action.Assign do
  alias __MODULE__
  alias Protean.{Interpreter, Action}

  defstruct [:merge]

  @type t :: %Assign{
          merge: %{any => any}
        }

  defimpl Action.Protocol.Executable, for: Assign do
    def exec(%Assign{merge: context}, _context, interpreter) do
      Interpreter.update_context(interpreter, &Map.merge(&1, context))
    end
  end
end
