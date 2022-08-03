defmodule Protean.TransitionTest do
  use ExUnit.Case

  alias Protean.Interpreter

  setup context do
    TestMachines.with_test_machine(context)
  end

  describe "automatic transitions" do
    @tag machine: :auto_transition_machine_1
    test "do not occur immediately", %{initial: initial} do
      assert Protean.matches?(initial, "a")
    end

    @tag machine: :auto_transition_machine_1
    test "occur when starting an interpreter", %{interpreter: interpreter} do
      assert Protean.matches?(interpreter, "a")
      interpreter = Interpreter.start(interpreter)
      assert Protean.matches?(interpreter, "b")
    end

    @tag machine: :auto_transition_machine_2
    test "trigger actions in correct order", %{interpreter: interpreter} do
      state =
        interpreter
        |> Interpreter.start()
        |> Interpreter.handle_event(:goto_b)
        |> Interpreter.state()

      assert Protean.matches?(state, :d)
      assert state.context[:acc] == ["auto_to_d", "auto_to_c"]
    end
  end
end
