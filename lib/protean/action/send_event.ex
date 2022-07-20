defmodule Protean.Action.SendEvent do
  @moduledoc "TODO"

  alias __MODULE__
  alias Protean.Action.Protocol.Executable
  alias Protean.Action.Protocol.Resolvable
  alias Protean.Interpreter.Server

  defmodule Resolved.Immediate do
    @moduledoc false

    defstruct [:event, :to]

    defimpl Executable, for: __MODULE__ do
      def exec(%{event: event, to: to}, interpreter) do
        Server.send_event_async(to, event)
        interpreter
      end
    end
  end

  defmodule Resolved.Delay do
    @moduledoc false

    defstruct [:event, :to, :delay]

    defimpl Executable, for: __MODULE__ do
      def exec(%{event: event, to: to, delay: delay}, interpreter) do
        Server.send_event_after(to, event, delay)
        interpreter
      end
    end
  end

  defmodule Unresolved do
    @moduledoc false

    defstruct [:event, :to, :delay]

    defimpl Resolvable, for: __MODULE__ do
      def resolve(%{to: to, event: event, delay: delay}, _state, _handler)
          when is_integer(delay) do
        %SendEvent.Resolved.Delay{event: event, delay: delay, to: recipient(to)}
      end

      def resolve(%{to: to, event: event}, _state, _handler),
        do: %SendEvent.Resolved.Immediate{event: event, to: recipient(to)}

      defp recipient(nil), do: self()
      defp recipient(:self), do: self()
      defp recipient(to), do: to
    end
  end
end
