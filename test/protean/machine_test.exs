defmodule Protean.MachineTest do
  use ExUnit.Case

  alias Protean.Machine

  setup context do
    if machine = context[:machine] do
      machine = apply(TestMachines, machine, [])
      {:ok, Map.put(context, :machine, machine)}
    else
      context
    end
  end

  @tag machine: :simple_machine_1
  test "machines have an initial state", %{machine: machine} do
    assert %Protean.State{} = Machine.initial_state(machine)
  end
end
