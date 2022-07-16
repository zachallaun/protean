defmodule Protean.Action.Assign do
  alias __MODULE__
  alias Protean.{Interpreter, Action.Executable}

  defstruct [:merge]

  @type t :: %Assign{
          merge: %{any => any}
        }

  defimpl Executable, for: Assign do
    def exec(%Assign{merge: context}, _context, interpreter) do
      Interpreter.update_context(interpreter, &Map.merge(&1, context))
    end
  end
end
