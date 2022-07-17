defmodule Protean.TransitionTest do
  use ExUnit.Case

  setup context do
    TestMachines.with_test_machine(context)
  end

  describe "automatic transitions from initial state" do
    @describetag machine: :auto_transition_machine_1

    test "do not occur immediately", %{initial: initial} do
      assert Protean.matches?(initial, "a")
    end

    test "occur when starting an interpreter", %{interpreter: interpreter} do
      assert Protean.matches?(interpreter, "a")
      interpreter = Protean.Interpreter.start(interpreter)
      assert Protean.matches?(interpreter, "b")
    end
  end
end
