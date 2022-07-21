defmodule Protean.NodeTest do
  use ExUnit.Case

  alias Protean.Node

  setup context do
    TestMachines.with_test_machine(context)
  end

  describe "resolving to leaves" do
    @describetag machine: :simple_machine_2

    test "atomic nodes resolve to themselves", %{machine: machine} do
      node = machine.idmap[["state_a1_child", "state_a1", "state_a", "#"]]
      assert Node.resolve_to_leaves(node) == [node]
    end

    test "final nodes resolve to themselves", %{machine: machine} do
      node = machine.idmap[["state_b", "#"]]
      assert Node.resolve_to_leaves(node) == [node]
    end

    test "compound nodes resolve based on their initial child", %{machine: machine} do
      node = machine.idmap[["state_a", "#"]]
      [resolved] = Node.resolve_to_leaves(node)

      assert resolved.id == ["state_a1_child", "state_a1", "state_a", "#"]
    end
  end
end
