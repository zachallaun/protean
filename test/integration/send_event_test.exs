defmodule ProteanIntegration.SendEventTest do
  use Protean.TestCase

  defmodule SendParent do
    use Protean
    alias Protean.Action

    @machine [
      initial: "waiting",
      states: [
        atomic(:waiting,
          on: [
            match({:echo, _}, actions: "echo")
          ]
        )
      ]
    ]

    @impl Protean
    def handle_action("echo", context, {:echo, echo}) do
      context
      |> Action.send({:echo, echo}, to: :parent)
    end
  end

  describe "SendParent:" do
    @describetag machine: SendParent

    test "sending event to: :parent", %{machine: machine} do
      Protean.call(machine, {:echo, :echo_back})

      assert_receive {:echo, :echo_back}
    end
  end
end
