defmodule Protean.Interpreter.Server do
  @moduledoc false

  import Kernel, except: [send: 2]

  use GenServer

  alias Protean.Interpreter

  @prefix :"$protean"

  @gen_server_options [:name, :timeout, :debug, :spawn_opt, :hibernate_after]

  # Client API

  def start_link(opts) do
    {gen_server_opts, interpreter_opts} = Keyword.split(opts, @gen_server_options)

    GenServer.start_link(__MODULE__, interpreter_opts, gen_server_opts)
  end

  def call(server, event, timeout) do
    GenServer.call(server, event, timeout)
  end

  def send(server, event) do
    server
    |> resolve_server_to_pid()
    |> Kernel.send(event)

    :ok
  end

  def send_after(server, event, time) do
    server
    |> resolve_server_to_pid()
    |> Process.send_after(event, time)
  end

  def current(pid) do
    GenServer.call(pid, {@prefix, :current_context})
  end

  def stop(pid, reason, timeout), do: GenServer.stop(pid, reason, timeout)

  def ping(pid), do: GenServer.call(pid, {@prefix, :ping})

  def fetch_id(pid) do
    case GenServer.whereis(pid) do
      nil -> {:error, :not_started}
      pid -> {:ok, GenServer.call(pid, {@prefix, :id})}
    end
  end

  defp resolve_server_to_pid(ref) when is_reference(ref), do: ref
  defp resolve_server_to_pid(server), do: GenServer.whereis(server)

  # GenServer callbacks

  @impl true
  def init(opts) do
    Process.flag(:trap_exit, true)
    {:ok, Interpreter.new(opts), {:continue, {@prefix, :start}}}
  end

  @impl true
  def handle_call({@prefix, :current_context}, _from, interpreter) do
    {:reply, Interpreter.context(interpreter), interpreter}
  end

  def handle_call({@prefix, :ping}, _from, interpreter) do
    {:reply, :ok, interpreter}
  end

  def handle_call({@prefix, :id}, _from, interpreter) do
    {:reply, interpreter.id, interpreter}
  end

  def handle_call(event, _from, interpreter) do
    {interpreter, response} = run_event(interpreter, event)
    {:reply, response, interpreter, {:continue, {@prefix, :check_running}}}
  end

  @impl true
  def handle_cast(event, interpreter) do
    {interpreter, _response} = run_event(interpreter, event)
    {:noreply, interpreter, {:continue, {@prefix, :check_running}}}
  end

  @impl true
  def handle_info(event, interpreter), do: handle_cast(event, interpreter)

  @impl true
  def handle_continue({@prefix, :check_running}, interpreter) do
    if Interpreter.running?(interpreter) do
      {:noreply, interpreter}
    else
      {:stop, {:shutdown, Interpreter.context(interpreter)}, interpreter}
    end
  end

  def handle_continue({@prefix, :start}, interpreter) do
    {:noreply, Interpreter.start(interpreter)}
  end

  @impl true
  def terminate(_reason, interpreter) do
    Interpreter.stop(interpreter)
  end

  defp run_event(interpreter, event) do
    {interpreter, replies} = Interpreter.handle_event(interpreter, event)
    {interpreter, {Interpreter.context(interpreter), replies}}
  end
end
