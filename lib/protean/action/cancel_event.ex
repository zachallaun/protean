defmodule Protean.Action.CancelEvent do
  @moduledoc "TODO"

  alias __MODULE__
  alias Protean.Action

  defstruct [:id]

  @type t :: %CancelEvent{
          id: term()
        }

  defimpl Action.Protocol.Resolvable, for: CancelEvent do
    def resolve(cancel_event, _, _, _), do: cancel_event
  end

  defimpl Action.Protocol.Executable, for: CancelEvent do
    def exec(%CancelEvent{}, _context, interpreter) do
      interpreter
    end
  end
end
