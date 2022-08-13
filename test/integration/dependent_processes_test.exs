defmodule ProteanIntegration.DependentProcessesTest do
  use Protean.TestCase

  defmodule Child do
    use Protean

    @machine [
      initial: "init",
      states: [atomic(:init)]
    ]
  end

  defmodule Parent do
    use Protean

    @machine [
      initial: "init",
      states: [
        atomic(:init,
          invoke: [
            proc: Child,
            id: "child"
          ]
        )
      ]
    ]
  end

  @tag machine: Parent
  test "child machines exit when their spawning process exits", %{machine: machine} do
    assert length(DynamicSupervisor.which_children(Protean.ProcessManager.Supervisor)) == 1
    :ok = Protean.stop(machine)
    :timer.sleep(50)
    assert length(DynamicSupervisor.which_children(Protean.ProcessManager.Supervisor)) == 0
  end
end
