defmodule Protean.MachineTest do
  use ExUnit.Case

  alias Protean.Machine

  setup context do
    TestMachines.with_test_machine(context)
  end

  describe "simple compound machine" do
    @describetag machine: :simple_machine_1

    test "has an initial state", %{machine: machine} do
      assert %Protean.State{} = Machine.initial_state(machine)
    end

    test "ignores unknown events", %{machine: machine, initial: initial} do
      maybe_different = Machine.transition(machine, initial, {"UNKNOWN_EVENT", nil})
      assert maybe_different == initial
    end

    test "transitions to atomic nodes", %{machine: machine, initial: initial} do
      next = Machine.transition(machine, initial, {"event_a", nil})
      assert next.value == [["state_b", "#"]]
    end
  end

  @tag machine: :simple_machine_2
  test "transitions when parent responds to event", %{machine: machine, initial: initial} do
    next = Machine.transition(machine, initial, {"event_a", nil})
    assert next.value == [["state_b", "#"]]
  end

  describe "simple parallel machine" do
    @describetag machine: :parallel_machine_1

    test "has an initial state", %{machine: machine} do
      assert Machine.initial_state(machine).value == [["state_a", "#"], ["state_b", "#"]]
    end
  end
end
