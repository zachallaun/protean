defmodule ProteanIntegration.RepliesTest do
  use Protean.TestCase

  defmodule Replying do
    use Protean

    @machine [
      initial: "init",
      states: [atomic(:init)],
      on: [
        {:no_reply, actions: [:no_response]},
        {:one_reply, actions: [{:respond, :one}]},
        {:two_replies, actions: [{:respond, :one}, {:respond, :two}]}
      ]
    ]

    @impl true
    def handle_action({:respond, response}, context, _) do
      {:reply, response, context}
    end

    def handle_action(:no_response, context, _) do
      {:noreply, context}
    end
  end

  describe "Replying:" do
    @describetag machine: Replying

    test "single response in an array", %{machine: machine} do
      assert {_, [:one]} = Protean.call(machine, :one_reply)
    end

    test "multiple responses come in action order", %{machine: machine} do
      assert {_, [:one, :two]} = Protean.call(machine, :two_replies)
    end

    test "subscriptions can be to transitions with replies only", %{machine: machine} do
      {:ok, id} = Protean.subscribe(machine, filter: :replies)

      Protean.call(machine, :one_reply)
      assert_receive {^id, _, [:one]}

      Protean.call(machine, :no_reply)
      refute_receive {^id, _, _}
    end
  end
end
