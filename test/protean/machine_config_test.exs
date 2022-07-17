defmodule Protean.MachineConfigTest do
  use ExUnit.Case

  setup context do
    TestMachines.with_test_machine(context)
  end

  @tag machines: [:delayed_transition_machine_implicit, :delayed_transition_machine_explicit]
  test "delayed transitions are just syntax sugar", %{machines: machines} do
    [%{machine: implicit}, %{machine: explicit}] = machines
    assert implicit.root == explicit.root
  end
end
