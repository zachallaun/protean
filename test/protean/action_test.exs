defmodule Protean.ActionTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog

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

    @impl true
    def handle_action(:invalid_return, _, _) do
      :should_raise
    end
  end

  defmodule MyAction do
    @behaviour Action

    @impl true
    def exec_action(:cont_with_empty_actions, interpreter) do
      {:cont, interpreter, []}
    end

    def exec_action(:cont_with_actions, interpreter) do
      {:cont, interpreter, [Action.assign(foo: :bar)]}
    end

    def exec_action(:cont_with_no_actions, interpreter) do
      {:cont, interpreter}
    end

    def exec_action(:halt, interpreter) do
      {:halt, interpreter}
    end

    def exec_action(:other, interpreter) do
      {:other, interpreter}
    end
  end

  setup do
    interpreter = Interpreter.new(machine: BasicMachine.__protean_machine__())
    [interpreter: interpreter, context: interpreter.context]
  end

  describe "exec/2:" do
    test "should normalize actions that return no actions", %{interpreter: interpreter} do
      result =
        MyAction
        |> Action.new(:cont_with_empty_actions)
        |> Action.exec(interpreter)

      assert {:cont, _interpreter} = result

      result =
        MyAction
        |> Action.new(:cont_with_no_actions)
        |> Action.exec(interpreter)

      assert {:cont, _interpreter} = result
    end

    test "should allow additional actions", %{interpreter: interpreter} do
      result =
        MyAction
        |> Action.new(:cont_with_actions)
        |> Action.exec(interpreter)

      assert {:cont, _interpreter, [_]} = result
    end

    test "should allow halting actions", %{interpreter: interpreter} do
      result =
        MyAction
        |> Action.new(:halt)
        |> Action.exec(interpreter)

      assert {:halt, _interpreter} = result
    end

    test "should raise for anything else", %{interpreter: interpreter} do
      action = Action.new(MyAction, :other)
      assert_raise RuntimeError, fn -> Action.exec(action, interpreter) end
    end
  end

  describe "put:" do
    test "put/2 should insert an action into context", %{context: context} do
      assert [] = Context.actions(context)
      context = Action.put(context, Action.assign(foo: :bar))
      assert [_] = Context.actions(context)
    end

    test "put/2 should raise if not given an action struct", %{context: context} do
      assert_raise FunctionClauseError, fn -> Action.put(context, :foo) end
    end
  end

  describe "delegate:" do
    test "delegate/2 should insert any action into context", %{context: context} do
      context = Action.delegate(context, :foo)
      assert [_] = Context.actions(context)
    end

    test "should log if callback returns invalid value", %{interpreter: interpreter} do
      message =
        capture_log(fn ->
          :invalid_return
          |> Action.delegate()
          |> Action.exec(interpreter)
        end)

      assert message =~ "invalid return"
    end
  end

  describe "assign:" do
    test "assign/1 should assign key/value data to context.assigns",
         %{interpreter: interpreter} do
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

  describe "assign_in:" do
    test "assign_in/3 should add an action to context", %{context: context} do
      assert [] = Context.actions(context)
      context = Action.assign_in(context, [:foo], :bar)
      assert [_] = Context.actions(context)
    end

    test "assign_in/2 should assign a value based on path", %{interpreter: interpreter} do
      interpreter =
        Action.exec_all(interpreter, [
          Action.assign(foo: 1, bar: 2),
          Action.assign_in([:foo], 11),
          Action.assign_in([:bar], fn -> :whatever end)
        ])

      %{foo: foo, bar: bar} = interpreter.context.assigns
      assert 11 = foo
      assert is_function(bar)
    end

    test "assign_in/2 should raise if path is empty list", %{interpreter: interpreter} do
      assert_raise FunctionClauseError,
                   fn -> Action.exec_all(interpreter, [Action.assign_in([], :foo)]) end
    end

    test "assign_in/2 should raise if path not a list", %{interpreter: interpreter} do
      assert_raise FunctionClauseError,
                   fn -> Action.exec_all(interpreter, [Action.assign_in(:foo, :bar)]) end
    end
  end

  describe "update:" do
    test "update/2 should assign an action to context", %{context: context} do
      assert [] = Context.actions(context)
      context = Action.update(context, fn -> %{foo: 1} end)
      assert [_] = Context.actions(context)
    end

    test "update/1 should apply an update fn to assigns", %{interpreter: interpreter} do
      interpreter =
        Action.exec_all(interpreter, [
          Action.assign(foo: 1),
          Action.update(fn %{foo: foo} -> %{foo: foo + 1} end)
        ])

      assert %{foo: 2} = interpreter.context.assigns
    end

    test "update/1 merges result into assigns", %{interpreter: interpreter} do
      interpreter =
        Action.exec_all(interpreter, [
          Action.assign(foo: 1),
          Action.update(fn _ -> %{bar: 2} end)
        ])

      assert %{foo: 1, bar: 2} = interpreter.context.assigns
    end
  end

  describe "update_in:" do
    test "update_in/3 should assign an action to context", %{context: context} do
      assert [] = Context.actions(context)
      context = Action.update_in(context, [:foo], fn foo -> foo + 1 end)
      assert [_] = Context.actions(context)
    end

    test "update_in/2 applies a function to value in path", %{interpreter: interpreter} do
      interpreter =
        Action.exec_all(interpreter, [
          Action.assign(foo: %{bar: %{baz: 1}}),
          Action.update_in([:foo, :bar, :baz], &(&1 + 1))
        ])

      assert %{foo: %{bar: %{baz: 2}}} = interpreter.context.assigns
    end
  end

  describe "send:" do
    test "send/2 should be able to send to self", %{interpreter: interpreter} do
      Action.exec_all(interpreter, [
        Action.send(:message1),
        Action.send(:message2, to: :self)
      ])

      assert_receive :message1
      assert_receive :message2
    end

    test "send/2 accepts an optional delay", %{interpreter: interpreter} do
      Action.exec_all(interpreter, [
        Action.send(:message, to: self(), delay: 0)
      ])

      assert_receive :message
    end
  end
end
