defmodule ProteanIntegration.SubscriptionTest do
  use Protean.TestCase, async: true

  defmodule TestMachine do
    use Protean

    @machine [
      initial: "a",
      states: [
        a: [
          on: [
            b: "b"
          ]
        ],
        b: [
          on: [
            a: "a"
          ]
        ]
      ]
    ]
  end

  @tag machine: TestMachine
  test "subscribed processes receive updates on transition", %{machine: machine} do
    ref = Protean.subscribe(machine)

    Protean.send_event(machine, "b")
    assert_receive {:state, _, ^ref}

    Protean.send_event(machine, "b")
    assert_receive {:state, _, ^ref}

    Protean.unsubscribe(machine, ref)

    Protean.send_event(machine, "a")
    refute_receive {:state, _}
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
