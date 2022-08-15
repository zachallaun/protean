defmodule Protean.ActionTest do
  use ExUnit.Case

  alias Protean.Action
  alias Protean.Context
  alias Protean.Interpreter

  defmodule BasicMachine do
    use Protean

    @machine [
      initial: :init,
      states: [
        atomic(:init)
      ]
    ]
  end

  setup do
    interpreter = Interpreter.new(machine: BasicMachine.__protean_machine__())
    [interpreter: interpreter, context: interpreter.context]
  end

  test "put/2 should insert an action into context", %{context: context} do
    assert [] = Context.actions(context)
    context = Action.put(context, Action.assign(:foo, :bar))
    assert [_] = Context.actions(context)
  end

  test "put/2 raises if not given an action struct", %{context: context} do
    assert_raise FunctionClauseError, fn -> Action.put(context, :foo) end
  end

  test "delegate/2 should insert any action into context", %{context: context} do
    context = Action.delegate(context, :foo)
    assert [_] = Context.actions(context)
  end
end
