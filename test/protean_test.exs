defmodule ProteanTest do
  use Protean.TestCase

  import ExUnit.CaptureLog

  test "should warn if unknown options are given to `use Protean`" do
    moduledef =
      quote do
        defmodule ShouldWarn do
          use Protean, unknown: :option

          @machine [
            initial: :init,
            states: [atomic(:init)]
          ]
        end
      end

    warning =
      capture_log(fn ->
        Code.eval_quoted(moduledef)
      end)

    assert warning =~ "unknown options"
  end

  defmodule SimpleMachine do
    use Protean

    @machine [
      initial: :init,
      states: [
        atomic(:init)
      ],
      on: [
        match(:reply, actions: :reply)
      ]
    ]

    @impl true
    def handle_action(:reply, state, _) do
      {:reply, :ok, state}
    end
  end

  describe "start_machine/2" do
    test "should return error if name already started" do
      {:ok, _, _} = Protean.start_machine(SimpleMachine, name: Machine)
      assert {:error, {:already_started, _}} = Protean.start_machine(SimpleMachine, name: Machine)

      Protean.stop(Machine, :normal, 10)
    end

    test "should return pid if name supplied" do
      {:ok, pid, _} = Protean.start_machine(SimpleMachine, name: Machine)
      assert is_pid(pid)

      Protean.stop(Machine)
    end

    test "should return pid if name not supplied" do
      {:ok, pid, _} = Protean.start_machine(SimpleMachine)
      assert is_pid(pid)

      Protean.stop(pid)
    end
  end

  describe "resolve/1" do
    @tag here: true
    test "should return pid if given a machine id" do
      {:ok, _, id} = Protean.start_machine(SimpleMachine)
      {:ok, pid} = Protean.resolve(id)
      assert is_pid(pid)

      Protean.stop(pid)
    end

    test "should return error if machine is stopped" do
      {:ok, pid, id} = Protean.start_machine(SimpleMachine)
      Protean.stop(pid)
      :error = Protean.resolve(id)
    end

    test "should return error if machine was never started" do
      :error = Protean.resolve("not-started")
    end

    test "should raise if an invalid id is given" do
      assert_raise FunctionClauseError, fn -> Protean.resolve(:foo) end
    end
  end

  describe "stop_machine/3" do
    test "should accept an optional reason" do
      assert {:ok, name, _} = Protean.start_machine(SimpleMachine)
      assert :ok = Protean.stop(name, :normal)
    end

    test "should accept an optional reason and timeout" do
      assert {:ok, name, _} = Protean.start_machine(SimpleMachine)
      assert :ok = Protean.stop(name, :normal, 50)
    end
  end

  describe "basic API usage" do
    @describetag machine: SimpleMachine

    test "call/2", %{machine: machine} do
      assert {%Protean.Context{}, []} = Protean.call(machine, "event")
    end

    test "send/2", %{machine: machine} do
      :ok = Protean.send(machine, "event")
    end

    test "send_after/3", %{machine: machine} do
      assert timer = Protean.send_after(machine, "event", 1000)
      assert 0 < Process.cancel_timer(timer)
    end

    test "current/1", %{machine: machine} do
      assert %Protean.Context{} = Protean.current(machine)
    end

    test "stop/2", %{machine: machine, ref: ref} do
      :ok = Protean.stop(machine, :default)
      assert_receive {:DOWN, ^ref, :process, _, {:shutdown, %Protean.Context{}}}
    end

    test "matches?/2", %{machine: machine} do
      assert Protean.matches?(machine, "init")
      assert Protean.current(machine) |> Protean.matches?("init")
    end
  end

  describe "subscribe/2" do
    @describetag machine: SimpleMachine

    test "should subscribe caller to transitions", %{machine: machine, id: id} do
      :ok = Protean.subscribe(id)
      Protean.call(machine, "event")
      assert_receive {^id, %Protean.Context{}, []}
    end

    test "can subscribe caller to only replies", %{machine: machine, id: id} do
      :ok = Protean.subscribe(id, filter: :replies)
      Protean.call(machine, "event")
      refute_receive {^id, _, _}
      Protean.call(machine, :reply)
      assert_receive {^id, _, [:ok]}
    end

    test "should raise if given a bad filter", %{id: id} do
      assert_raise ArgumentError, fn -> Protean.subscribe(id, filter: :bad) end
    end
  end

  describe "unsubscribe/1" do
    @describetag machine: SimpleMachine

    test "should unsubscribe calling process from transition", %{machine: machine, id: id} do
      :ok = Protean.subscribe(id)
      :ok = Protean.unsubscribe(id)
      assert Protean.call(machine, "event")
      refute_receive {^id, _, _}
    end
  end

  defmodule MachineWithoutCallbacks do
    use Protean, callback_module: ProteanTest.CallbackModule

    @machine [
      initial: "init",
      states: [
        atomic(:init, entry: :my_action)
      ]
    ]
  end

  defmodule CallbackModule do
    def handle_action(:my_action, state, _) do
      state
      |> Protean.Action.assign(:data, :foo)
    end
  end

  @tag machine: MachineWithoutCallbacks
  test "separate callback_module can be specified", %{machine: machine} do
    assert_protean(machine,
      assigns: [data: :foo]
    )
  end

  defmodule DefaultAssigns do
    use Protean

    @machine [
      assigns: %{data: :foo},
      initial: "init",
      states: [
        atomic(:init)
      ]
    ]
  end

  describe "machines with assigns:" do
    test "started with default assigns" do
      {:ok, pid} = start_supervised(DefaultAssigns)
      assert Protean.current(pid).assigns == %{data: :foo}
    end

    test "started with replacement assigns" do
      {:ok, pid} = start_supervised({DefaultAssigns, assigns: %{data: :bar}})
      assert Protean.current(pid).assigns == %{data: :bar}
    end

    test "started with added assigns" do
      {:ok, pid} = start_supervised({DefaultAssigns, assigns: %{bar: :baz}})
      assert Protean.current(pid).assigns == %{data: :foo, bar: :baz}
    end
  end

  test "Phoenix.PubSub is optional" do
    {stdout, 0} =
      System.cmd("mix", ["deps.compile", "--force"],
        cd: "./test/support/dependent_project",
        stderr_to_stdout: true
      )

    refute stdout =~ "warning: Phoenix.PubSub"
    refute stdout =~ "warning: "
  end
end
