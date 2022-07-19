defmodule Protean.Interpreter.ServerTest do
  use ProteanTest.Case

  alias Protean.Interpreter.Server
  alias Protean.State

  defmodule TestMachine do
    use Protean

    @machine [
      initial: :a,
      states: [
        a: [
          on: [
            goto_b: :b
          ]
        ],
        b: []
      ]
    ]
  end

  @tag machine: TestMachine
  test "server can be started and stopped", %{machine: server, ref: ref} do
    assert Server.matches?(server, :a)
    assert :ok = Server.stop(server)
    assert_receive {:DOWN, ^ref, :process, _, :normal}
  end

  @tag machine: {TestMachine, name: NamedMachine}
  test "server can be started with a name" do
    assert %State{} = Server.current(NamedMachine)
  end

  @tag machine: TestMachine
  test "send/2", %{machine: server} do
    assert state = %State{} = Server.send(server, "goto_b")
    assert State.matches?(state, :b)
  end

  @tag machine: TestMachine
  test "send_async/2", %{machine: server} do
    assert :ok = Server.send_async(server, "goto_b")
    assert Server.matches?(server, :b)
  end

  @tag machine: TestMachine
  test "send_after/3", %{machine: server} do
    Server.send_after(server, "goto_b", 10)
    :timer.sleep(20)
    assert Server.matches?(server, :b)
  end

  @tag machine: TestMachine
  test "send_after/3 can be canceled", %{machine: server} do
    timer = Server.send_after(server, "goto_b", 10)
    Process.cancel_timer(timer)
    :timer.sleep(20)
    assert Server.matches?(server, :a)
  end
end
