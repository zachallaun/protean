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
    context = Action.put(context, Action.assign(foo: :bar))
    assert [_] = Context.actions(context)
  end

  test "put/2 raises if not given an action struct", %{context: context} do
    assert_raise FunctionClauseError, fn -> Action.put(context, :foo) end
  end

  test "delegate/2 should insert any action into context", %{context: context} do
    context = Action.delegate(context, :foo)
    assert [_] = Context.actions(context)
  end

  describe "assigns in context" do
    test "assign/1", %{interpreter: interpreter} do
      interpreter =
        Action.exec_all(interpreter, [
          Action.assign(foo: nil, bar: 2),
          Action.assign(foo: 1),
          Action.assign(%{baz: 3}),
          Action.assign([{"buzz", 4}])
        ])

      assert %{"buzz" => 4, foo: 1, bar: 2, baz: 3} = interpreter.context.assigns
    end
  end
end
