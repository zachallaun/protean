defmodule Protean.Interpreter.ServerTest do
  use Protean.TestCase

  alias Protean.Interpreter.Server
  alias Protean.Context

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

  test "server can be started and stopped" do
    {:ok, name} = Protean.start_machine(TestMachine)
    pid = GenServer.whereis(name)
    assert Server.matches?(name, :a)
    assert :ok = Server.stop(name, :normal, :infinity)
    refute Process.alive?(pid)
  end

  test "server can be started with a name" do
    {:ok, _} = Protean.start_machine(TestMachine, name: NamedMachine)
    assert %Context{} = Server.current(NamedMachine)
  end

  @tag machine: TestMachine
  test "call/3", %{machine: server} do
    assert {context = %Context{}, []} = Server.call(server, :goto_b, 5000)
    assert Context.matches?(context, :b)
  end

  @tag machine: TestMachine
  test "send/2", %{machine: server} do
    assert :ok = Server.send(server, :goto_b)
    assert Server.matches?(server, :b)
  end

  @tag machine: TestMachine
  test "send_after/3", %{machine: server} do
    Server.send_after(server, :goto_b, 10)
    :timer.sleep(20)
    assert Server.matches?(server, :b)
  end

  @tag machine: TestMachine
  test "send_after/3 can be canceled", %{machine: server} do
    timer = Server.send_after(server, :goto_b, 10)
    Process.cancel_timer(timer)
    :timer.sleep(20)
    assert Server.matches?(server, :a)
  end
end
