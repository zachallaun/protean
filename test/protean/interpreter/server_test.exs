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
    {:ok, pid} = start_supervised(TestMachine)
    assert Protean.matches?(pid, :a)
    assert :ok = Server.stop(pid, :normal, :infinity)
    refute Process.alive?(pid)
  end

  test "server can be started with a name" do
    {:ok, _} = start_supervised({TestMachine, name: NamedMachine})
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
    assert Protean.matches?(server, :b)
  end

  @tag machine: TestMachine
  test "send_after/3", %{machine: server} do
    Server.send_after(server, :goto_b, 10)
    :timer.sleep(20)
    assert Protean.matches?(server, :b)
  end

  @tag machine: TestMachine
  test "send_after/3 can be canceled", %{machine: server} do
    timer = Server.send_after(server, :goto_b, 10)
    Process.cancel_timer(timer)
    :timer.sleep(20)
    assert Protean.matches?(server, :a)
  end
end
