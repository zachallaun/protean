defmodule Protean.InterpreterTest do
  use ExUnit.Case

  alias Protean.Interpreter

  setup context do
    TestMachines.with_test_machine(context)
  end

  describe "purely functional machine interpreter" do
    @describetag machine: :pure_machine_1

    test "can be created and started", %{machine: machine, handler: handler} do
      interpreter = Interpreter.new(machine, handler)
      assert !Interpreter.running?(interpreter)
      assert interpreter |> Interpreter.start() |> Interpreter.running?()
    end

    test "executes initial entry actions on start", %{interpreter: interpreter} do
      assert Enum.count(interpreter.state.actions) == 1

      with interpreter <- Interpreter.start(interpreter) do
        assert Enum.empty?(interpreter.state.actions)
        assert interpreter.state.context[:acc] == ["entering_a"]
      end
    end
  end

  describe "machine with basic guards" do
    @describetag machine: :silly_direction_machine

    test "transitions based on guard conditions", %{interpreter: interpreter} do
      with interpreter <- Interpreter.start(interpreter) do
        interpreter = Interpreter.send_event(interpreter, "go")
        assert Interpreter.state(interpreter).value == [["straight", "#"]]

        interpreter =
          interpreter
          |> Interpreter.send_event("set_left")
          |> Interpreter.send_event("go")

        assert Interpreter.state(interpreter).value == [["left", "#"]]
      end
    end
  end
end
