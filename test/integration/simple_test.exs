defmodule ProteanIntegration.SimpleTest do
  use Protean.TestCase

  defmodule SimpleMachine do
    use Protean

    @machine [
      assigns: [
        data: nil
      ],
      initial: "a",
      states: [
        atomic(:a,
          on: [goto_b: "b"]
        ),
        atomic(:b,
          on: [goto_a: "a"]
        )
      ],
      on: [
        match({:set_data, _}, actions: "do_set")
      ]
    ]

    @impl true
    def handle_action("do_set", context, {_, data}) do
      context
      |> Protean.Action.assign(:data, data)
    end
  end

  @moduletag machine: SimpleMachine

  test "SimpleMachine", %{machine: machine} do
    assert_protean(machine,
      matches: "a",
      call: :goto_b,
      matches: "b",
      assigns: [data: nil],
      call: {:set_data, :ok},
      assigns: [data: :ok],
      call: :goto_a,
      matches: "a",
      assigns: [data: :ok]
    )
  end
end
