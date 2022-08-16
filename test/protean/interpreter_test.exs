defmodule Protean.InterpreterTest do
  use ExUnit.Case

  alias Protean.Interpreter
  alias Protean.Context

  setup context do
    TestMachines.with_test_machine(context)
  end

  describe "purely functional machine interpreter" do
    @describetag machine: :pure_machine_1

    test "can be created and started", %{machine: machine} do
      interpreter = Interpreter.new(machine: machine)
      assert !Interpreter.running?(interpreter)
      assert interpreter |> Interpreter.start() |> Interpreter.running?()
    end

    test "can be started twice to no effect", %{machine: machine} do
      interpreter = Interpreter.new(machine: machine)
      assert interpreter |> Interpreter.start() |> Interpreter.start() |> Interpreter.running?()
    end

    test "executes initial entry actions on start", %{interpreter: interpreter} do
      assert Enum.count(Context.actions(interpreter.context)) == 1

      with interpreter <- Interpreter.start(interpreter) do
        assert Enum.empty?(Context.actions(interpreter.context))
        assert interpreter.context.assigns[:acc] == ["entering_a"]
      end
    end
  end

  describe "machine with basic guards" do
    @describetag machine: :silly_direction_machine

    test "transitions based on guard conditions", %{interpreter: interpreter} do
      with interpreter <- Interpreter.start(interpreter) do
        {interpreter, _} = Interpreter.handle_event(interpreter, :go)
        assert Interpreter.context(interpreter).value == MapSet.new([["straight", "#"]])

        {interpreter, _} = Interpreter.handle_event(interpreter, :set_left)
        {interpreter, _} = Interpreter.handle_event(interpreter, :go)

        assert Interpreter.context(interpreter).value == MapSet.new([["left", "#"]])
      end
    end
  end
end
