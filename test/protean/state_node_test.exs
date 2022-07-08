defmodule Protean.StateNodeTest do
  use ExUnit.Case

  alias Protean.StateNode

  setup context do
    TestMachines.with_test_machine(context)
  end

  describe "resolving to leaves" do
    @describetag machine: :simple_machine_2

    test "atomic nodes resolve to themselves", %{machine: machine} do
      node = machine.idmap[["state_a1_child", "state_a1", "state_a", "#"]]
      assert StateNode.resolve_to_leaf(node) == node
    end

    test "final nodes resolve to themselves", %{machine: machine} do
      node = machine.idmap[["state_b", "#"]]
      assert StateNode.resolve_to_leaf(node) == node
    end

    test "compound nodes resolve based on their initial child", %{machine: machine} do
      node = machine.idmap[["state_a", "#"]]
      resolved = StateNode.resolve_to_leaf(node)

      assert resolved.id == ["state_a1_child", "state_a1", "state_a", "#"]
    end
  end
end
