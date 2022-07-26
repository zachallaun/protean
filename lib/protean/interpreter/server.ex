defmodule Protean.Interpreter.Server do
  @moduledoc """
  `Protean.Interpreter.Server` implements the behaviour required to run a `Protean.Interpreter`
  as a `GenServer`. This module is usually not invoked directly, but instead defines a client API
  that is delegated to by `Protean`.
  """

  use GenServer

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
  def send_event_async(pid, event) do
    pid
    |> GenServer.whereis()
    |> send(Protean.event(event))

    :ok
  end

  @doc """
  Send an event to the interpreter after `time` in milliseconds has passed. Returns a timer
  reference that can be canceled with `Process.cancel_timer/1`.
  """
  @spec send_event_after(server, Protean.sendable_event(), non_neg_integer) :: reference
  def send_event_after(pid, event, time) do
    pid
    |> GenServer.whereis()
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
  @spec matches?(server, descriptor :: any) :: boolean
  def matches?(pid, pattern) do
    pid
    |> current()
    |> State.matches?(pattern)
  end

  @doc """
  Stop the service, terminating the process.
  """
  @spec stop(server, reason :: term) :: :ok
  def stop(pid, reason \\ :normal) do
    GenServer.stop(pid, reason)
    :ok
  end

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
    reply_with_state(interpreter)
  end

  def handle_call({:event, event}, _from, interpreter) do
    interpreter
    |> Interpreter.send_event(event)
    |> reply_with_state()
  end

  defp reply_with_state(interpreter) do
    {:reply, Interpreter.state(interpreter), interpreter}
  end

  @impl true
  def handle_cast({:event, event}, interpreter) do
    {:noreply, interpreter, {:continue, {:event, event}}}
  end

  @impl true
  def handle_continue({:event, event}, interpreter) do
    {:noreply, Interpreter.send_event(interpreter, event)}
  end

  @impl true
  def handle_info({event_name, _} = event, interpreter) when is_binary(event_name) do
    {:noreply, Interpreter.send_event(interpreter, event)}
  end

  def handle_info({:DOWN, ref, :process, _pid, _reason}, interpreter) do
    {:noreply, Interpreter.notify_process_down(interpreter, ref)}
  end

  def handle_info(anything, interpreter) do
    require Logger
    Logger.info("Unexpected message: #{inspect(anything)}")
    {:noreply, interpreter}
  end

  @impl true
  def terminate(_reason, interpreter) do
    Interpreter.stop(interpreter)
    :ok
  end
end
