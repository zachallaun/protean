defmodule ProteanIntegration.AutoforwardTest do
  use Protean.TestCase

  defmodule Child do
    use Protean

    @machine [
      initial: "receiving",
      context: [data: []],
      states: [
        receiving: [
          on: [
            {match({"event", _}), actions: :log_data}
          ]
        ]
      ]
    ]

    @impl true
    def action(:log_data, state, {_, value}) do
      Protean.Action.assign_in(state, [:data], &[value | &1])
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

    assert %{data: ["c", "b", "a"]} = Protean.current(ChildMachine).context
  end
end
