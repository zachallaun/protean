defmodule ProteanIntegration.AutoforwardTest do
  use Protean.TestCase

  defmodule Child do
    use Protean

    @machine [
      initial: "receiving",
      assigns: [data: []],
      states: [
        receiving: [
          on: [
            {match({"event", _}), actions: :log_data}
          ]
        ]
      ]
    ]

    @impl true
    def handle_action(:log_data, context, {_, value}) do
      Protean.Action.assign_in(context, [:data], &[value | &1])
    end
  end

  defmodule Parent do
    use Protean

    @machine [
      initial: "forwarding",
      states: [
        forwarding: [
          invoke: [
            proc: {Child, name: ChildMachine},
            id: "child",
            autoforward: true
          ]
        ]
      ]
    ]
  end

  @tag machine: Parent
  test "events are forwarded to invoked children with autoforward: true", %{machine: machine} do
    Protean.call(machine, {"event", "a"})
    Protean.call(machine, {"event", "b"})
    Protean.call(machine, {"event", "c"})

    assert %{data: ["c", "b", "a"]} = Protean.current(ChildMachine).assigns
  end
end
