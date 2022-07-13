defprotocol Protean.Action.Executable do
  @moduledoc """
  The `Executable` protocol defines `exec/2`, which is used to execute a
  resolved action that has been bound to a specific context and potentially
  modify an interpreter.
  """

  alias Protean.{Action, Machine, Interpreter}

  @spec exec(Action.resolved(), Machine.context(), Interpreter.t()) :: Interpreter.t()
  def exec(action, context, interpreter)
end
