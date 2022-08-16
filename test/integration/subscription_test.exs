defmodule ProteanIntegration.SubscriptionTest do
  use Protean.TestCase, async: true

  defmodule TestMachine do
    use Protean

    @machine [
      initial: "a",
      states: [
        atomic(:a,
          on: [
            {"b", target: "b", actions: :answer}
          ]
        ),
        atomic(:b,
          on: [
            {"a", target: "a"}
          ]
        )
      ]
    ]

    @impl true
    def handle_action(:answer, context, _) do
      {:reply, :answer, context}
    end
  end

  describe "subscribed processes" do
    @describetag machine: TestMachine

    test "should receive updates on transition", %{machine: machine, id: id} do
      :ok = Protean.subscribe(id)

      Protean.call(machine, "b")
      assert_receive {^id, %Protean.Context{}, _}

      Protean.call(machine, "b")
      assert_receive {^id, %Protean.Context{}, _}

      Protean.unsubscribe(id)

      Protean.call(machine, "a")
      refute_receive {^id, %Protean.Context{}, _}
    end

    test "can subscribe only to transitions with replies", %{machine: machine, id: id} do
      :ok = Protean.subscribe(id, filter: :replies)

      Protean.call(machine, "b")
      assert_receive {^id, _, [:answer]}

      Protean.call(machine, "a")
      refute_receive {^id, _, _}
    end

    test "should receive two updates when subscribed twice", %{machine: machine, id: id} do
      :ok = Protean.subscribe(id)
      :ok = Protean.subscribe(id)

      Protean.call(machine, "b")
      assert_receive {^id, _, _}
      assert_receive {^id, _, _}

      :ok = Protean.unsubscribe(id)

      Protean.call(machine, "b")
      refute_receive {^id, _, _}
    end
  end
end
