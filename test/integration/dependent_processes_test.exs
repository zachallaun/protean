defmodule ProteanIntegration.DependentProcessesTest do
  use Protean.TestCase

  defmodule Child do
    use Protean

    @machine [
      initial: "init",
      states: [init: []]
    ]
  end

  defmodule Parent do
    use Protean

    @machine [
      initial: "init",
      states: [
        init: [
          invoke: [
            proc: Child,
            id: "child"
          ]
        ]
      ]
    ]
  end

  @tag machine: Parent
  test "child machines exit when their spawning process exits", %{machine: machine} do
    assert length(DynamicSupervisor.which_children(Protean)) == 1
    :ok = Protean.stop(machine)
    :timer.sleep(50)
    assert length(DynamicSupervisor.which_children(Protean)) == 0
  end
end
