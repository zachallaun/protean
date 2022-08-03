defmodule Protean.InterpreterTest do
  use ExUnit.Case

  alias Protean.Interpreter
  alias Protean.State

  setup context do
    TestMachines.with_test_machine(context)
  end

  describe "purely functional machine interpreter" do
    @describetag machine: :pure_machine_1

    test "can be created and started", %{machine: machine, handler: handler} do
      interpreter = Interpreter.new(machine: machine, handler: handler)
      assert !Interpreter.running?(interpreter)
      assert interpreter |> Interpreter.start() |> Interpreter.running?()
    end

    test "executes initial entry actions on start", %{interpreter: interpreter} do
      assert Enum.count(State.actions(interpreter.state)) == 1

      with interpreter <- Interpreter.start(interpreter) do
        assert Enum.empty?(State.actions(interpreter.state))
        assert interpreter.state.context[:acc] == ["entering_a"]
      end
    end
  end

  describe "machine with basic guards" do
    @describetag machine: :silly_direction_machine

    test "transitions based on guard conditions", %{interpreter: interpreter} do
      with interpreter <- Interpreter.start(interpreter) do
        interpreter = Interpreter.handle_event(interpreter, :go)
        assert Interpreter.state(interpreter).value == MapSet.new([["straight", "#"]])

        interpreter =
          interpreter
          |> Interpreter.handle_event(:set_left)
          |> Interpreter.handle_event(:go)

        assert Interpreter.state(interpreter).value == MapSet.new([["left", "#"]])
      end
    end
  end

  @tag machine: :higher_order_guard_machine_1
  test "higher order guards", %{interpreter: interpreter} do
    higher_order_guard_test(interpreter)
  end

  @tag machine: :higher_order_guard_machine_2
  test "higher order guards with syntax sugar", %{interpreter: interpreter} do
    higher_order_guard_test(interpreter)
  end

  def higher_order_guard_test(interpreter) do
    with interpreter <- Interpreter.start(interpreter) do
      assert Protean.matches?(interpreter, "a")

      interpreter = Interpreter.handle_event(interpreter, :goto_c)
      assert Protean.matches?(interpreter, "a")

      interpreter = Interpreter.handle_event(interpreter, :goto_b)
      assert Protean.matches?(interpreter, "b")

      interpreter = Interpreter.handle_event(interpreter, {:goto_d, nil})
      assert Protean.matches?(interpreter, "b")

      interpreter = Interpreter.handle_event(interpreter, :goto_c)
      assert Protean.matches?(interpreter, "c")

      interpreter = Interpreter.handle_event(interpreter, {:goto_d, nil})
      assert Protean.matches?(interpreter, "c")

      interpreter = Interpreter.handle_event(interpreter, {:goto_d, :please})
      assert Protean.matches?(interpreter, "d")

      interpreter = Interpreter.handle_event(interpreter, :goto_a)
      assert Protean.matches?(interpreter, "d")

      interpreter = Interpreter.handle_event(interpreter, :goto_c)
      assert Protean.matches?(interpreter, "c")

      interpreter = Interpreter.handle_event(interpreter, :goto_a)
      assert Protean.matches?(interpreter, "a")
    end
  end
end
