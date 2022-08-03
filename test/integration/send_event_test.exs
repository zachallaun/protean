defmodule ProteanIntegration.SendEventTest do
  use Protean.TestCase

  defmodule SendParent do
    use Protean
    alias Protean.Action

    defmachine(
      initial: "waiting",
      states: [
        waiting: [
          on: [
            {{:echo, _}, actions: "echo"}
          ]
        ]
      ]
    )

    @impl Protean
    def action("echo", state, {:echo, echo}) do
      state
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
