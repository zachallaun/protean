defprotocol Protean.Action.Protocol.Executable do
  @moduledoc """
  The `Executable` protocol defines `exec/2` which is used to execute previously-resolved actions
  and update the interpreter.
  """

  alias Protean.Action
  alias Protean.Interpreter

  @spec exec(Action.resolved(), Interpreter.t()) :: Interpreter.t() | nil
  def exec(action, interpreter)
end
