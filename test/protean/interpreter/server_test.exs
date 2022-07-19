defmodule Protean.Interpreter.ServerTest do
  use ExUnit.Case

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

  setup context do
    if server = context[:server] do
      setup_server(server)
    end
  end

  @tag server: TestMachine
  test "server can be started and stopped", %{server: server, monitor_ref: ref} do
    assert Server.matches?(server, :a)
    assert :ok = Server.stop(server)
    assert_receive {:DOWN, ^ref, :process, _, :normal}
  end

  @tag server: {TestMachine, name: NamedMachine}
  test "server can be started with a name" do
    assert %State{} = Server.current(NamedMachine)
  end

  @tag server: TestMachine
  test "send/2", %{server: server} do
    assert state = %State{} = Server.send(server, "goto_b")
    assert State.matches?(state, :b)
  end

  @tag server: TestMachine
  test "send_async/2", %{server: server} do
    assert :ok = Server.send_async(server, "goto_b")
    assert Server.matches?(server, :b)
  end

  @tag server: TestMachine
  test "send_after/3", %{server: server} do
    Server.send_after(server, "goto_b", 10)
    :timer.sleep(20)
    assert Server.matches?(server, :b)
  end

  @tag server: TestMachine
  test "send_after/3 can be canceled", %{server: server} do
    timer = Server.send_after(server, "goto_b", 10)
    Process.cancel_timer(timer)
    :timer.sleep(20)
    assert Server.matches?(server, :a)
  end

  defp setup_server({module, opts}) do
    {:ok, pid} = module.start_link(opts)
    ref = Process.monitor(pid)

    on_exit(fn -> Process.exit(pid, :normal) end)

    [server: pid, monitor_ref: ref]
  end

  defp setup_server(module), do: setup_server({module, []})
end
