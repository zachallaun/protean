defmodule Protean.Action.CancelEvent do
  @moduledoc "TODO"

  alias __MODULE__
  alias Protean.Action.Protocol.Executable
  alias Protean.Action.Protocol.Resolvable

  defmodule Resolved do
    @moduledoc false

    defstruct [:id]

    defimpl Executable, for: __MODULE__ do
      # TODO:
      def exec(%{id: _id}, interpreter),
        do: interpreter
    end
  end

  defmodule Unresolved do
    @moduledoc false

    defstruct [:id]

    defimpl Resolvable, for: __MODULE__ do
      def resolve(%{id: id}, _state, _handler),
        do: %CancelEvent.Resolved{id: id}
    end
  end
end
