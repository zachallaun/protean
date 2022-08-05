defmodule ProteanIntegration.SubscriptionTest do
  use Protean.TestCase, async: true

  defmodule TestMachine do
    use Protean

    @machine [
      initial: "a",
      states: [
        a: [
          on: [
            {"b", target: "b", actions: :answer}
          ]
        ],
        b: [
          on: [
            {"a", target: "a"}
          ]
        ]
      ]
    ]

    @impl true
    def action(:answer, state, _) do
      Protean.Action.answer(state, :answer)
    end
  end

  @tag machine: TestMachine
  test "subscribed processes receive updates on transition", %{machine: machine} do
    ref = Protean.subscribe(machine)

    Protean.call(machine, "b")
    assert_receive {:state, _, _, ^ref}

    Protean.call(machine, "b")
    assert_receive {:state, _, _, ^ref}

    Protean.unsubscribe(machine, ref)

    Protean.call(machine, "a")
    refute_receive {:state, _, _, _}
  end

  @tag machine: TestMachine
  test "processes can subscribe only to transitions with an answer", %{machine: machine} do
    ref = Protean.subscribe(machine, to: :answer)

    Protean.call(machine, "b")
    assert_receive {:state, _, {:ok, :answer}, ^ref}

    Protean.call(machine, "a")
    refute_receive {:state, _, _, _}
  end

  @tag machine: TestMachine
  test "subscribed processes receive :DOWN when machine terminates", %{machine: machine} do
    ref = Protean.subscribe(machine)
    :ok = Protean.stop(machine)
    assert_receive {:DOWN, ^ref, :process, _, _}
  end

  @tag machine: TestMachine
  test "subscribing to a dead process immediately sends :DOWN", %{machine: machine} do
    ref = Protean.subscribe(machine)
    :ok = Protean.stop(machine)
    assert_receive {:DOWN, ^ref, :process, _, _}
  end
end
