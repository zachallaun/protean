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
      maybe_different = Machine.transition(machine, initial, "UNKNOWN_EVENT")
      assert maybe_different == initial
    end

    test "transitions to atomic nodes", %{machine: machine, initial: initial} do
      next = Machine.transition(machine, initial, "event_a")
      assert next.value == [["state_b", "#"]]
    end
  end

  @tag machine: :simple_machine_2
  test "transitions when parent responds to event", %{machine: machine, initial: initial} do
    next = Machine.transition(machine, initial, "event_a")
    assert next.value == [["state_b", "#"]]
  end

  describe "simple parallel machine" do
    @describetag machine: :parallel_machine_1

    test "has an initial state", %{machine: machine} do
      assert Machine.initial_state(machine).value == [["state_a", "#"], ["state_b", "#"]]
    end
  end

  describe "machine with basic actions" do
    @describetag machine: :machine_with_actions_1

    test "entry actions are collected in initial state", %{initial: initial} do
      assert initial.actions == ["entry_a"]
    end

    test "exit and entry actions are collected on transition", %{
      machine: machine,
      initial: initial
    } do
      state = Machine.transition(machine, initial, "event_a")
      assert state.value == [["state_b", "#"]]
      assert state.actions == ["entry_a", "exit_a", "event_a_action", "entry_b"]
    end
  end

  describe "parallel machine with actions" do
    @describetag machine: :parallel_machine_with_actions_1

    test "has correct initial state and entry actions", %{initial: initial} do
      assert initial.value == [
               ["state_a1", "parallel_state_a", "#"],
               ["foo", "state_a2", "parallel_state_a", "#"]
             ]

      assert initial.actions == ["entry_parallel_a", "entry_a1", "entry_a2"]
    end

    test "can transition within a parallel state", %{machine: machine, initial: initial} do
      state = Machine.transition(machine, initial, "foo_event")

      assert state.value == [
               ["state_a1", "parallel_state_a", "#"],
               ["bar", "state_a2", "parallel_state_a", "#"]
             ]

      assert state.actions == initial.actions ++ ["exit_foo", "entry_bar"]
    end
  end
end
