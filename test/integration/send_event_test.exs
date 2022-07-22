defmodule ProteanIntegration.SendEventTest do
  use Protean.TestCase

  defmodule SendParent do
    use Protean
    alias Protean.Action

    @machine [
      initial: "waiting",
      states: [
        waiting: [
          on: [
            echo: [
              actions: ["echo"]
            ]
          ]
        ]
      ]
    ]

    @impl Protean
    def pure("echo", state, {"echo", echo}) do
      state
      |> Action.send_event({"echo", echo}, to: :parent)
    end
  end

  describe "SendParent:" do
    @describetag machine: SendParent

    test "sending event to: :parent", %{machine: machine} do
      Protean.send_event(machine, {"echo", :echo_back})

      assert_receive {:"$gen_cast", {:event, {"echo", :echo_back}}}
    end
  end
end
