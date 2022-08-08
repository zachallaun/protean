defmodule Protean.TransitionTest do
  use ExUnit.Case

  alias Protean.Transition
  import Protean.TestHelper

  describe "internal transition domains" do
    test "to self" do
      t = transition("#.compound", ["#.compound"], true)
      assert Transition.domain(t) == node_id("#")
    end

    test "to child" do
      t = transition("#.compound", ["#.compound.child"], true)
      assert Transition.domain(t) == node_id("#.compound")
    end

    test "to sibling" do
      t = transition("#.compound", ["#.other"], true)
      assert Transition.domain(t) == node_id("#")
    end

    test "to self and child" do
      t = transition("#.compound", ["#.compound", "#.compound.parallel.child"], true)
      assert Transition.domain(t) == node_id("#")
    end

    test "to multiple children" do
      t = transition("#.c", ["#.c.p.child1", "#.c.p.child2"], true)
      assert Transition.domain(t) == node_id("#.c")
    end

    test "to child of sibling" do
      t = transition("#.c1.a", ["#.c2.a"], true)
      assert Transition.domain(t) == node_id("#")
    end
  end

  describe "external transition domains" do
    test "to self" do
      t = transition("#.compound", ["#.compound"])
      assert Transition.domain(t) == node_id("#")
    end

    test "to child" do
      t = transition("#.compound", ["#.compound.child"])
      assert Transition.domain(t) == node_id("#")
    end

    test "to sibling" do
      t = transition("#.compound", ["#.other"])
      assert Transition.domain(t) == node_id("#")
    end

    test "to self and child" do
      t = transition("#.compound", ["#.compound", "#.compound.parallel.child"])
      assert Transition.domain(t) == node_id("#")
    end

    test "to multiple children" do
      t = transition("#.c", ["#.c.p.child1", "#.c.p.child2"])
      assert Transition.domain(t) == node_id("#")
    end

    test "to child of sibling" do
      t = transition("#.c1.a", ["#.c2.a"], true)
      assert Transition.domain(t) == node_id("#")
    end
  end

  defp transition(source_id, target_ids, internal \\ false) when is_list(target_ids) do
    Transition.new(
      source_id: node_id(source_id),
      target_ids: Enum.map(target_ids, &node_id/1),
      internal: internal
    )
  end
end
