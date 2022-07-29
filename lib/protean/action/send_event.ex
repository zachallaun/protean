defmodule Protean.Action.SendEvent do
  @moduledoc "TODO"

  alias __MODULE__
  alias Protean.Action.Protocol.Executable
  alias Protean.Action.Protocol.Resolvable
  alias Protean.Interpreter.Server

  def to(_interpreter, nil), do: self()
  def to(_interpreter, :self), do: self()
  def to(%{parent: parent}, :parent), do: parent

  def to(%{invoked: invoked}, to) when is_binary(to) do
    case invoked[to] do
      %{pid: pid} -> pid
    end
  end

  def to(_interpreter, to), do: to

  defmodule Resolved.Immediate do
    @moduledoc false

    defstruct [:event, :to]

    defimpl Executable do
      def exec(%{event: event, to: to}, interpreter) do
        interpreter
        |> SendEvent.to(to)
        |> Server.send_event_async(event)

        interpreter
      end
    end
  end

  defmodule Resolved.Delay do
    @moduledoc false

    defstruct [:event, :to, :delay]

    defimpl Executable do
      def exec(%{event: event, to: to, delay: delay}, interpreter) do
        interpreter
        |> SendEvent.to(to)
        |> Server.send_event_after(event, delay)

        interpreter
      end
    end
  end

  defmodule Unresolved do
    @moduledoc false

    defstruct [:event, :to, :delay]

    defimpl Resolvable do
      def resolve(%{to: to, event: event, delay: delay}, _state, _handler)
          when is_integer(delay) do
        %SendEvent.Resolved.Delay{event: event, delay: delay, to: to}
      end

      def resolve(%{to: to, event: event}, _state, _handler),
        do: %SendEvent.Resolved.Immediate{event: event, to: to}
    end
  end
end
