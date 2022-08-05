defmodule ProteanIntegration.CustomActionTest do
  use Protean.TestCase

  defmodule CustomAction do
    @behaviour Protean.Action

    def custom do
      Protean.Action.new(__MODULE__, :custom)
    end

    def exec_action(:custom, interpreter) do
      {:halt, interpreter}
    end
  end

  defmodule TestMachine do
    use Protean

    @machine [
      initial: "init",
      context: [data: nil],
      states: [
        init: [
          entry: [
            CustomAction.custom(),
            Protean.Action.assign(data: :ok)
          ],
          on: [
            event: []
          ]
        ]
      ]
    ]
  end

  @tag machine: TestMachine
  test "custom action halts action pipeline", %{machine: machine} do
    assert_protean(machine,
      context: [data: nil],
      call: "event",
      context: [data: nil]
    )
  end
end
