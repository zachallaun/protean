defmodule Protean.Action.Assign do
  @moduledoc "TODO"

  alias __MODULE__
  alias Protean.Action.Protocol.Executable
  alias Protean.Action.Protocol.Resolvable
  alias Protean.State

  defmodule Resolved do
    @moduledoc false

    defstruct [:merge]

    defimpl Executable, for: __MODULE__ do
      def exec(%{merge: context}, interpreter),
        do: update_in(interpreter.state, &State.assign(&1, context))
    end
  end

  defmodule Unresolved do
    @moduledoc false

    defstruct [:merge]

    defimpl Resolvable, for: __MODULE__ do
      def resolve(%{merge: merge}, _state, _handler),
        do: %Assign.Resolved{merge: merge}
    end
  end
end
