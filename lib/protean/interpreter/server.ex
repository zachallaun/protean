defmodule Protean.Interpreter.Server do
  @moduledoc false

  import Kernel, except: [send: 2]

  use GenServer

  alias Protean.Interpreter
  alias Protean.State

  @prefix :"$protean"

  @type server_options :: [Interpreter.option() | GenServer.option()]

  @gen_server_options [:name, :timeout, :debug, :spawn_opt, :hibernate_after]

  # Client API

  @doc false
  @spec start_link(server_options) :: GenServer.on_start()
  def start_link(opts) do
    {gen_server_opts, interpreter_opts} = Keyword.split(opts, @gen_server_options)

    GenServer.start_link(__MODULE__, interpreter_opts, gen_server_opts)
  end

  @doc false
  @spec call(GenServer.server(), Protean.event(), timeout()) :: State.t()
  def call(pid, event, timeout), do: GenServer.call(pid, event, timeout)
  def call(pid, event), do: GenServer.call(pid, event)

  @doc false
  @spec send(GenServer.server(), Protean.event()) :: :ok
  def send(server, event) do
    server
    |> resolve_server_to_pid()
    |> Kernel.send(event)

    :ok
  end

  @doc false
  @spec send_after(GenServer.server(), Protean.event(), non_neg_integer()) :: reference()
  def send_after(server, event, time) do
    server
    |> resolve_server_to_pid()
    |> Process.send_after(event, time)
  end

  @doc false
  @spec current(GenServer.server()) :: State.t()
  def current(pid) do
    GenServer.call(pid, {@prefix, :current_state})
  end

  @doc """
  Get the current machine state and check whether it matches the given descriptor. See
  `Protean.State.matches?/2` for descriptor usage.
  """
  @spec matches?(GenServer.server(), descriptor :: term()) :: boolean()
  def matches?(pid, pattern) do
    pid
    |> current()
    |> State.matches?(pattern)
  end

  @doc """
  Stop the service, terminating the process.
  """
  @spec stop(GenServer.server(), reason :: term()) :: :ok
  def stop(pid, reason \\ :default)
  def stop(pid, :default), do: GenServer.stop(pid, {:shutdown, current(pid)})
  def stop(pid, reason), do: GenServer.stop(pid, reason)

  def subscribe(server, monitor: true) do
    ref =
      server
      |> resolve_server_to_pid()
      |> Process.monitor()

    GenServer.cast(server, {@prefix, :subscribe, self(), ref})

    ref
  end

  def subscribe(server, monitor: false) do
    ref = make_ref()
    GenServer.cast(server, {@prefix, :subscribe, self(), ref})
    ref
  end

  def unsubscribe(server, ref) do
    GenServer.cast(server, {@prefix, :unsubscribe, self(), ref})
  end

  @doc false
  def ping(pid), do: GenServer.call(pid, {@prefix, :ping})

  defp resolve_server_to_pid(ref) when is_reference(ref), do: ref
  defp resolve_server_to_pid(server), do: GenServer.whereis(server)

  # GenServer callbacks

  @impl true
  def init(opts) do
    Process.flag(:trap_exit, true)

    interpreter =
      Interpreter.new(opts)
      |> Interpreter.start()

    {:ok, interpreter}
  end

  @impl true
  def handle_call({@prefix, :current_state}, _from, interpreter) do
    {:reply, Interpreter.state(interpreter), interpreter}
  end

  def handle_call({@prefix, :ping}, _from, interpreter) do
    {:reply, :ok, interpreter}
  end

  def handle_call(event, _from, interpreter) do
    interpreter = Interpreter.handle_event(interpreter, event)
    {:reply, Interpreter.state(interpreter), interpreter, {:continue, {@prefix, :check_running}}}
  end

  @impl true
  def handle_cast({@prefix, :subscribe, pid, ref}, interpreter) do
    {:noreply, Interpreter.subscribe(interpreter, {pid, ref}),
     {:continue, {@prefix, :check_running}}}
  end

  def handle_cast({@prefix, :unsubscribe, pid, ref}, interpreter) do
    {:noreply, Interpreter.unsubscribe(interpreter, {pid, ref}),
     {:continue, {@prefix, :check_running}}}
  end

  def handle_cast(event, interpreter) do
    {:noreply, Interpreter.handle_event(interpreter, event),
     {:continue, {@prefix, :check_running}}}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _pid, reason}, interpreter) do
    {:noreply, Interpreter.notify_process_down(interpreter, reason, ref: ref),
     {:continue, {@prefix, :check_running}}}
  end

  def handle_info({:EXIT, _pid, reason}, interpreter) do
    {:stop, reason, interpreter}
  end

  def handle_info(event, interpreter) do
    {:noreply, Interpreter.handle_event(interpreter, event),
     {:continue, {@prefix, :check_running}}}
  end

  @impl true
  def handle_continue({@prefix, :check_running}, interpreter) do
    if Interpreter.running?(interpreter) do
      {:noreply, interpreter}
    else
      {:stop, {:shutdown, Interpreter.state(interpreter)}, interpreter}
    end
  end

  @impl true
  def terminate(_reason, interpreter) do
    Interpreter.stop(interpreter)
  end
end
