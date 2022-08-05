defmodule ProteanIntegration.SimpleTest do
  use Protean.TestCase

  defmodule SimpleMachine do
    use Protean

    @machine [
      context: [
        data: nil
      ],
      initial: "a",
      states: [
        a: [
          on: [goto_b: "b"]
        ],
        b: [
          on: [goto_a: "a"]
        ]
      ],
      on: [
        {match({:set_data, _}), actions: "do_set"}
      ]
    ]

    @impl true
    def action("do_set", state, {_, data}) do
      state
      |> Protean.Action.assign(:data, data)
    end
  end

  @moduletag machine: SimpleMachine

  test "SimpleMachine", %{machine: machine} do
    assert_protean(machine,
      matches: "a",
      call: :goto_b,
      matches: "b",
      context: [data: nil],
      call: {:set_data, :ok},
      context: [data: :ok],
      call: :goto_a,
      matches: "a",
      context: [data: :ok]
    )
  end
end
