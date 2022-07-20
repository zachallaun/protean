defmodule ProteanIntegration.SimpleTest do
  use Protean.TestCase

  defmodule SimpleMachine do
    use Protean

    @machine [
      context: [
        data: nil
      ],
      initial: :a,
      states: [
        a: [
          on: [goto_b: :b]
        ],
        b: [
          on: [goto_a: :a]
        ]
      ],
      on: [
        set_data: [
          actions: ["set_data"]
        ]
      ]
    ]

    @impl true
    def pure("set_data", %{event: {_, data}} = state, _context) do
      state
      |> Protean.Action.assign(:data, data)
    end
  end

  @moduletag machine: SimpleMachine

  test "SimpleMachine", %{machine: machine} do
    assert_protean(machine, [
      {:matches?, :a},
      {:send, "goto_b"},
      {:matches?, :b},
      {:context, data: nil},
      {:send, "set_data", :ok},
      {:context, data: :ok},
      {:send, "goto_a"},
      {:matches?, :a},
      {:context, data: :ok}
    ])
  end
end
