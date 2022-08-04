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
      start_supervised(Protean.Supervisor)
      assert Supervisor.which_children(Protean.Supervisor)
    end

    test "can be started with an explicit name" do
      start_supervised({Protean.Supervisor, name: MyProtean})
      assert Supervisor.which_children(MyProtean)
    end

    test "used by default to start invoked processes" do
      start_supervised(Protean.Supervisor)
      {:ok, _} = TimerMachine.start_link()
      assert 1 = length(Supervisor.which_children(Protean.Supervisor))
    end

    test "can be explicitly passed to machine" do
      start_supervised({Protean.Supervisor, name: MyProtean})
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

  defmodule MachineWithoutCallbacks do
    use Protean, callback_module: ProteanTest.CallbackModule

    defmachine(
      initial: "init",
      states: [
        init: [
          entry: :my_action
        ]
      ]
    )
  end

  defmodule CallbackModule do
    use Protean

    @impl true
    def action(:my_action, state, _) do
      state
      |> Protean.Action.assign(:data, :foo)
    end
  end

  @tag machine: MachineWithoutCallbacks
  test "separate callback_module can be specified", %{machine: machine} do
    assert_protean(machine,
      context: [data: :foo]
    )
  end

  defmodule Outer do
    use Protean

    defmachine(Inner1,
      initial: "init",
      states: [
        init: [
          entry: :first_action
        ]
      ]
    )

    defmachine(Inner2,
      initial: "init",
      states: [
        init: [
          entry: :second_action
        ]
      ]
    )

    @impl true
    def action(action, state, _) do
      state
      |> Protean.Action.assign(:data, action)
    end
  end

  @tag machines: [Outer.Inner1, Outer.Inner2]
  test "defmachine/2 defines nested modules with a shared callback module", context do
    [%{machine: inner1}, %{machine: inner2}] = context[:machines]

    assert_protean(inner1, context: [data: :first_action])
    assert_protean(inner2, context: [data: :second_action])
  end

  test "otp callbacks only defined if defmachine/1 is used" do
    refute Kernel.function_exported?(Outer, :start_link, 1)
  end

  defmodule DefaultContext do
    use Protean

    defmachine(
      context: %{data: :foo},
      initial: "init",
      states: [init: []]
    )
  end

  describe "machines with context:" do
    test "started with default context" do
      {:ok, pid} = DefaultContext.start_link()
      assert Protean.current(pid).context == %{data: :foo}
    end

    test "started with replacement context" do
      {:ok, pid} = DefaultContext.start_link(context: %{data: :bar})
      assert Protean.current(pid).context == %{data: :bar}
    end

    test "started with added context" do
      {:ok, pid} = DefaultContext.start_link(context: %{bar: :baz})
      assert Protean.current(pid).context == %{data: :foo, bar: :baz}
    end
  end
end
