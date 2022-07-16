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
  end
end
