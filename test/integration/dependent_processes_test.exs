defmodule ProteanIntegration.DependentProcessesTest do
  # Cannot be async because test looks into internal Protean.ProcessManager.Supervisor that is
  # shared with other tests.
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
          spawn: [
            proc(Child, id: "child")
          ]
        )
      ]
    ]
  end

  @tag machine: Parent
  test "child machines exit when their spawning process exits", %{machine: machine} do
    n = length(DynamicSupervisor.which_children(Protean.ProcessManager.Supervisor))
    :ok = Protean.stop(machine)
    :timer.sleep(20)
    assert length(DynamicSupervisor.which_children(Protean.ProcessManager.Supervisor)) == n - 2
  end
end
