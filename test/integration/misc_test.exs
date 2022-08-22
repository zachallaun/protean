defmodule ProteanIntegration.MiscTest do
  use Protean.TestCase, async: true

  @moduletag trigger: MiscTrigger

  defmodule TestMachine do
    use Protean

    @machine [
      initial: :init,
      states: [
        atomic(:init,
          on: [
            match(_, actions: Trigger.action(MiscTrigger, :received_event))
          ]
        )
      ]
    ]
  end

  @tag machine: TestMachine
  test "wildcard match shouldn't match init platform event", %{machine: machine} do
    Protean.matches?(machine, :init)
    refute Trigger.triggered?(MiscTrigger, :received_event)
  end
end
