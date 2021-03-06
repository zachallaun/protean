defmodule Protean.Action.Assign do
  @moduledoc "TODO"

  alias __MODULE__
  alias Protean.Action.Protocol.Executable
  alias Protean.Action.Protocol.Resolvable
  alias Protean.State

  defmodule Resolved.Merge do
    @moduledoc false

    defstruct [:merge]

    defimpl Executable, for: __MODULE__ do
      def exec(%{merge: context}, interpreter),
        do: update_in(interpreter.state, &State.assign(&1, context))
    end
  end

  defmodule Resolved.Update do
    @moduledoc false

    defstruct [:function]

    defimpl Executable, for: __MODULE__ do
      def exec(%{function: function}, %{state: state} = interpreter) do
        updates = function.(state, state.context, state.event)
        update_in(interpreter.state, &State.assign(&1, updates))
      end
    end
  end

  defmodule Unresolved do
    @moduledoc false

    defstruct [:merge, :function]

    defimpl Resolvable, for: __MODULE__ do
      def resolve(%{merge: merge, function: nil}, _state, _handler),
        do: %Assign.Resolved.Merge{merge: merge}

      def resolve(%{function: fun, merge: nil}, _state, _handler),
        do: %Assign.Resolved.Update{function: fun}
    end
  end
end
