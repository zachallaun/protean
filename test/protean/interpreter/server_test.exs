defmodule Protean.Interpreter.ServerTest do
  use ExUnit.Case

  alias Protean.Interpreter.Server
  alias Protean.State

  defmodule TestMachine do
    use Protean,
      machine: [
        initial: :a,
        states: [
          a: []
        ]
      ]
  end

  test "server can be started and stopped" do
    {:ok, pid} = TestMachine.start_link()
    ref = Process.monitor(pid)

    assert %State{} = Server.current(pid)
    assert :ok = Server.stop(pid)
    assert_receive {:DOWN, ^ref, :process, _, :normal}
  end

  test "server can be started with a name" do
    {:ok, _pid} = TestMachine.start_link(name: NamedMachine)

    assert %State{} = Server.current(NamedMachine)
    assert :ok = Server.stop(NamedMachine)
  end
end
