defmodule Protean.Interpreter.Server do
  @moduledoc """
  `Protean.Interpreter.Server` implements the behaviour required to run a `Protean.Interpreter`
  as a `GenServer`. This module is usually not invoked directly, but instead defines a client API
  that is delegated to by `Protean`.
  """

  use GenServer

  alias Protean.{Interpreter}

  @protean_event "$protean.event"
  @protean_snapshot "$protean.snapshot"
  @protean_terminate "$protean.terminate"

  @type server :: GenServer.server()

  @type server_options :: [Interpreter.option() | gen_server_options]

  @type gen_server_options :: {:gen_server, GenServer.options()}

  # Client API

  @doc """
  Start a new server. Accepts the following options:

    * `:machine` (required) - The `Protean.Machine` defining the behavior of the state machine.
    * `:handler` (required) - The module defining action handlers.
    * `:gen_server` (optional) - A keyword list of options to be passed to
      `GenServer.start_link/3`. For more information, see [those docs](https://hexdocs.pm/elixir/GenServer.html#start_link/3).
  """
  @spec start_link(server_options) :: GenServer.on_start()
  def start_link(opts) do
    {gen_server_opts, interpreter_opts} = Keyword.pop(opts, :gen_server, [])
    GenServer.start_link(__MODULE__, interpreter_opts, gen_server_opts)
  end

  @doc """
  Send an event to the interpreter and wait for the next state.
  """
  @spec send(server(), Interpreter.sendable()) :: State.t()
  def send(pid, event) do
    GenServer.call(pid, {@protean_event, event})
  end

  @doc """
  Send an event to the interpreter asyncronously.
  """
  @spec send_async(server(), Interpreter.sendable()) :: :ok
  def send_async(pid, event) do
    GenServer.cast(pid, {@protean_event, event})
  end

  @doc """
  Get the current machine state.
  """
  @spec current(server()) :: State.t()
  def current(pid) do
    GenServer.call(pid, @protean_snapshot)
  end

  @doc """
  Stop the service, terminating the process.
  """
  @spec stop(server()) :: :ok
  def stop(pid) do
    GenServer.cast(pid, @protean_terminate)
  end

  # GenServer callbacks

  @impl true
  def init(opts) do
    machine = Keyword.fetch!(opts, :machine)
    handler = Keyword.fetch!(opts, :handler)

    interpreter =
      Interpreter.new(machine, handler)
      |> Interpreter.start()

    {:ok, interpreter}
  end

  @impl true
  def handle_call(@protean_snapshot, _from, interpreter) do
    {:reply, Interpreter.state(interpreter), interpreter}
  end

  def handle_call({@protean_event, event}, _from, interpreter) do
    interpreter = Interpreter.send_event(interpreter, event)
    {:reply, interpreter.state, interpreter}
  end

  @impl true
  def handle_cast({@protean_event, event}, interpreter) do
    {:noreply, interpreter, {:continue, {@protean_event, event}}}
  end

  def handle_cast(@protean_terminate, interpreter) do
    {:stop, :normal, interpreter}
  end

  @impl true
  def handle_continue({@protean_event, event}, interpreter) do
    {:noreply, Interpreter.send_event(interpreter, event)}
  end

  @impl true
  def terminate(:normal, interpreter) do
    Interpreter.stop(interpreter)
    :ok
  end
end
