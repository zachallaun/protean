defmodule Protean.Interpreter.Server do
  @moduledoc """
  `Protean.Interpreter.Server` implements the behaviour required to run a `Protean.Interpreter`
  as a `GenServer`. This module is usually not invoked directly, but instead defines a client API
  that is delegated to by `Protean`.
  """

  use GenServer

  require Logger

  alias Protean.Interpreter
  alias Protean.State

  @type server :: GenServer.server()

  @type server_options :: [Interpreter.option() | GenServer.option()]

  @gen_server_options [:name, :timeout, :debug, :spawn_opt, :hibernate_after]

  # Client API

  @doc """
  Start a new server. Accepts the following options:

    * `:machine` (required) - The `Protean.Machine` defining the behavior of the state machine.
    * `:handler` (required) - The module defining action handlers.
    * `:parent` - The pid or `{pid, node}` of the parent process that will receive events from
      the running machine if a send-to-parent action occurs or when the machine reaches a final
      state. Defaults to `self()`.
    * GenServer options - All other options will be passed to `GenServer.start_link/3`.
  """
  @spec start_link(server_options) :: GenServer.on_start()
  def start_link(opts) do
    {gen_server_opts, interpreter_opts} = Keyword.split(opts, @gen_server_options)

    interpreter_opts = Keyword.put_new(interpreter_opts, :parent, self())

    GenServer.start_link(__MODULE__, interpreter_opts, gen_server_opts)
  end

  @doc """
  Send an event to the interpreter and wait for the next state.
  """
  @spec send_event(server, Protean.sendable_event()) :: State.t()
  def send_event(pid, event) do
    GenServer.call(pid, {:event, Protean.event(event)})
  end

  @doc """
  Send an event to the interpreter asyncronously.
  """
  @spec send_event_async(server, Protean.sendable_event()) :: :ok
  def send_event_async(server, event) do
    server
    |> resolve_server_to_pid()
    |> send(Protean.event(event))

    :ok
  end

  @doc """
  Send an event to the interpreter after `time` in milliseconds has passed. Returns a timer
  reference that can be canceled with `Process.cancel_timer/1`.
  """
  @spec send_event_after(server, Protean.sendable_event(), non_neg_integer()) :: reference()
  def send_event_after(server, event, time) do
    server
    |> resolve_server_to_pid()
    |> Process.send_after(Protean.event(event), time)
  end

  @doc """
  Get the current machine state.
  """
  @spec current(server) :: State.t()
  def current(pid) do
    GenServer.call(pid, :current_state)
  end

  @doc """
  Get the current machine state and check whether it matches the given descriptor. See
  `Protean.State.matches?/2` for descriptor usage.
  """
  @spec matches?(server, descriptor :: term()) :: boolean()
  def matches?(pid, pattern) do
    pid
    |> current()
    |> State.matches?(pattern)
  end

  @doc """
  Stop the service, terminating the process.
  """
  @spec stop(server, reason :: term()) :: :ok
  def stop(pid, reason \\ :default)
  def stop(pid, :default), do: GenServer.stop(pid, {:shutdown, current(pid)})
  def stop(pid, reason), do: GenServer.stop(pid, reason)

  @doc false
  def ping(pid), do: GenServer.call(pid, :ping)

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
  def handle_call(:current_state, _from, interpreter) do
    {:reply, Interpreter.state(interpreter), interpreter}
  end

  def handle_call(:ping, _from, interpreter) do
    {:reply, :ok, interpreter}
  end

  def handle_call({:event, event}, _from, interpreter) do
    interpreter = Interpreter.send_event(interpreter, event)
    {:reply, Interpreter.state(interpreter), interpreter, {:continue, :check_running}}
  end

  @impl true
  def handle_cast({:event, event}, interpreter) do
    {:noreply, interpreter, {:continue, {:event, event}}}
  end

  @impl true
  def handle_continue({:event, event}, interpreter) do
    {:noreply, Interpreter.send_event(interpreter, event), {:continue, :check_running}}
  end

  def handle_continue(:check_running, interpreter) do
    if Interpreter.running?(interpreter) do
      {:noreply, interpreter}
    else
      {:stop, {:shutdown, Interpreter.state(interpreter)}, interpreter}
    end
  end

  @impl true
  def handle_info({event_name, _} = event, interpreter) when is_binary(event_name) do
    {:noreply, Interpreter.send_event(interpreter, event), {:continue, :check_running}}
  end

  def handle_info({:DOWN, ref, :process, _pid, _reason}, interpreter) do
    {:noreply, Interpreter.notify_process_down(interpreter, ref: ref)}
  end

  def handle_info({:EXIT, _pid, :normal}, interpreter) do
    {:noreply, interpreter}
  end

  def handle_info(anything, interpreter) do
    Logger.info("Unexpected message: #{inspect(anything)}")
    {:noreply, interpreter}
  end

  @impl true
  def terminate(_reason, interpreter) do
    Interpreter.stop(interpreter)
    :ok
  end
end
