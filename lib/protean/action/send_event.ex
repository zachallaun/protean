defmodule Protean.Action.SendEvent do
  @moduledoc "TODO"

  alias __MODULE__
  alias Protean.Action.Protocol.Executable
  alias Protean.Action.Protocol.Resolvable
  alias Protean.Interpreter
  alias Protean.Machine

  defstruct [:event, :to, :delay]

  @type t :: %SendEvent{
          event: Machine.sendable_event(),
          to: Process.dest() | nil,
          delay: non_neg_integer | nil
        }

  defimpl Resolvable, for: SendEvent do
    def resolve(send_event, _state, _handler), do: send_event
  end

  defimpl Executable, for: SendEvent do
    def exec(action, interpreter) do
      action
      |> recipient()
      |> send_event_to(action)

      interpreter
    end

    defp recipient(%{to: nil}), do: self()
    defp recipient(%{to: :self}), do: self()
    defp recipient(%{to: to}), do: to

    defp send_event_to(to, %{event: event, delay: delay}) when is_integer(delay),
      do: Interpreter.Server.send_after(to, event, delay)

    defp send_event_to(to, %{event: event}),
      do: Interpreter.Server.send_async(to, event)
  end
end
