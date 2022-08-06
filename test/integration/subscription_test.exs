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
    def handle_action(:answer, state, _) do
      {:reply, :answer, state}
    end
  end

  @tag machine: TestMachine
  test "subscribed processes receive updates on transition", %{machine: machine} do
    ref = Protean.subscribe(machine)

    Protean.call(machine, "b")
    assert_receive {:state, ^ref, {%Protean.State{}, _}}

    Protean.call(machine, "b")
    assert_receive {:state, ^ref, {%Protean.State{}, _}}

    Protean.unsubscribe(machine, ref)

    Protean.call(machine, "a")
    refute_receive {:state, ^ref, _}
  end

  @tag machine: TestMachine
  test "processes can subscribe only to transitions with replies", %{machine: machine} do
    ref = Protean.subscribe(machine, :replies)

    Protean.call(machine, "b")
    assert_receive {:state, ^ref, {_, [:answer]}}

    Protean.call(machine, "a")
    refute_receive {:state, ^ref, _}
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
