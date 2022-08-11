defmodule Protean.MachineryTest do
  use ExUnit.Case

  alias Protean.Machinery

  setup context do
    TestMachines.with_test_machine(context)
  end

  describe "simple compound machine" do
    @describetag machine: :simple_machine_1

    test "ignores unknown events", %{machine: machine, initial: initial} do
      maybe_different = Machinery.transition(machine, initial, "UNKNOWN_EVENT")
      assert maybe_different == initial
    end

    test "transitions to atomic nodes", %{machine: machine, initial: initial} do
      next = Machinery.transition(machine, initial, :event_a)
      assert next.value == MapSet.new([["state_b", "#"]])
    end
  end

  @tag machine: :simple_machine_2
  test "transitions when parent responds to event", %{machine: machine, initial: initial} do
    next = Machinery.transition(machine, initial, :event_a)
    assert next.value == MapSet.new([["state_b", "#"]])
  end

  describe "machine with basic actions" do
    @describetag machine: :machine_with_actions_1

    test "entry actions are collected in initial state", %{initial: initial} do
      assert initial.private.actions == ["entry_a"]
    end

    test "exit and entry actions are collected on transition", %{
      machine: machine,
      initial: initial
    } do
      context = Machinery.transition(machine, initial, :event_a)
      assert context.value == MapSet.new([["state_b", "#"]])
      assert context.private.actions == ["entry_a", "exit_a", "event_a_action", "entry_b"]
    end
  end

  describe "parallel machine with actions" do
    @describetag machine: :parallel_machine_with_actions_1

    test "has correct initial state and entry actions", %{initial: initial} do
      assert initial.value ==
               MapSet.new([
                 ["state_a1", "parallel_state_a", "#"],
                 ["foo", "state_a2", "parallel_state_a", "#"]
               ])

      assert initial.private.actions == ["entry_parallel_a", "entry_a1", "entry_a2"]
    end

    test "can transition within a parallel state", %{machine: machine, initial: initial} do
      context = Machinery.transition(machine, initial, :foo_event)

      assert context.value ==
               MapSet.new([
                 ["state_a1", "parallel_state_a", "#"],
                 ["bar", "state_a2", "parallel_state_a", "#"]
               ])

      assert context.private.actions == initial.private.actions ++ ["exit_foo", "entry_bar"]
    end
  end
end
