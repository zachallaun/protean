defmodule Protean.Action.SendEvent do
  @moduledoc "TODO"

  alias __MODULE__
  alias Protean.{Interpreter, Machine, Action}

  defstruct [:event, :to, :delay]

  @type t :: %SendEvent{
          event: Machine.sendable_event(),
          to: Process.dest() | nil,
          delay: non_neg_integer | nil
        }

  defimpl Action.Protocol.Resolvable, for: SendEvent do
    def resolve(send_event, _, _, _), do: send_event
  end

  defimpl Action.Protocol.Executable, for: SendEvent do
    def exec(%SendEvent{to: nil} = action, context, interpreter) do
      exec(%{action | to: self()}, context, interpreter)
    end

    def exec(%SendEvent{event: event, to: to, delay: delay}, _context, interpreter)
        when is_integer(delay) do
      Interpreter.Server.send_after(to, event, delay)
      interpreter
    end

    def exec(%SendEvent{event: event, to: to}, _context, interpreter) do
      Interpreter.Server.send_async(to, event)
      interpreter
    end
  end
end
