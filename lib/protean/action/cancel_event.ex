defmodule Protean.Action.CancelEvent do
  @moduledoc "TODO"

  alias __MODULE__
  alias Protean.Action.Protocol.Executable
  alias Protean.Action.Protocol.Resolvable

  defstruct [:id]

  @type t :: %CancelEvent{
          id: term()
        }

  defimpl Resolvable, for: CancelEvent do
    def resolve(cancel_event, _state, _handler), do: cancel_event
  end

  defimpl Executable, for: CancelEvent do
    def exec(%CancelEvent{}, interpreter) do
      # TODO:
      interpreter
    end
  end
end
