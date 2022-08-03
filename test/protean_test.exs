defmodule ProteanTest do
  use Protean.TestCase

  defmodule SimpleMachine do
    use Protean

    defmachine(
      initial: "init",
      states: [init: []]
    )
  end

  defmodule TimerMachine do
    use Protean

    defmachine(
      initial: "init",
      states: [
        init: [
          invoke: [
            task: {:timer, :sleep, [100]}
          ]
        ]
      ]
    )
  end

  describe "Protean supervisor" do
    test "can be started with default name" do
      start_supervised(Protean)
      assert Supervisor.which_children(Protean)
    end

    test "can be started with an explicit name" do
      start_supervised({Protean, name: MyProtean})
      assert Supervisor.which_children(MyProtean)
    end

    test "used by default to start invoked processes" do
      start_supervised(Protean)
      {:ok, _} = TimerMachine.start_link()
      assert 1 = length(Supervisor.which_children(Protean))
    end

    test "can be explicitly passed to machine" do
      start_supervised({Protean, name: MyProtean})
      {:ok, _} = TimerMachine.start_link(supervisor: MyProtean)
      assert 1 = length(Supervisor.which_children(MyProtean))
    end
  end

  describe "basic API usage" do
    @describetag machine: SimpleMachine

    test "call/2", %{machine: machine} do
      assert %Protean.State{} = Protean.call(machine, "event")
    end

    test "send/2", %{machine: machine} do
      assert :ok = Protean.send(machine, "event")
    end

    test "send_after/3", %{machine: machine} do
      assert timer = Protean.send_after(machine, "event", 1000)
      assert 0 < Process.cancel_timer(timer)
    end

    test "current/1", %{machine: machine} do
      assert %Protean.State{} = Protean.current(machine)
    end

    test "stop/2", %{machine: machine} do
      assert :ok = Protean.stop(machine, :default)
      assert_receive {:EXIT, ^machine, {:shutdown, %Protean.State{}}}
    end

    test "subscribe/2", %{machine: machine} do
      assert ref = Protean.subscribe(machine)
      assert Protean.call(machine, "event")
      assert_receive {:state, %Protean.State{}, ^ref}
    end

    test "unsubscribe/2", %{machine: machine} do
      assert ref = Protean.subscribe(machine)
      assert :ok = Protean.unsubscribe(machine, ref)
      assert Protean.call(machine, "event")
      refute_receive {:state, %Protean.State{}, ^ref}
    end

    test "matches?/2", %{machine: machine} do
      assert Protean.matches?(machine, "init")
      assert Protean.current(machine) |> Protean.matches?("init")
    end
  end
end
