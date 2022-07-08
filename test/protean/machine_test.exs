defmodule Protean.MachineTest do
  use ExUnit.Case

  alias Protean.Machine

  setup context do
    if machine = context[:machine] do
      machine = apply(TestMachines, machine, [])

      context =
        context
        |> Map.put(:machine, machine)
        |> Map.put(:initial, Machine.initial_state(machine))

      {:ok, context}
    else
      context
    end
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
  end
end