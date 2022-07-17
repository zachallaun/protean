defmodule Protean.Interpreter.Server do
  @moduledoc """
  `Protean.Interpreter.Server` implements the behaviour required to run a `Protean.Interpreter`
  as a `GenServer`. This module is usually not invoked directly, but instead defines a client API
  that is delegated to by `Protean`.
  """

  use GenServer

  alias Protean.{Interpreter, Machine}

  @protean_snapshot "$protean.snapshot"
  @protean_terminate "$protean.terminate"

  @type server :: GenServer.server()

  @type server_options :: [Interpreter.option() | gen_server_options]

  @type gen_server_options :: {:gen_server, GenServer.options()}

  @type server_event ::
          {:event, name :: Machine.event_name(), data :: term}
          | {name :: Machine.event_name(), data :: term}
          | Machine.event_name()

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
  @spec send(server(), server_event()) :: State.t()
  def send(pid, event) do
    GenServer.call(pid, normalize_event(event))
  end

  @doc """
  Send an event to the interpreter asyncronously.
  """
  @spec send_async(server(), server_event()) :: :ok
  def send_async(pid, event) do
    GenServer.cast(pid, normalize_event(event))
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

  defp normalize_event({:event, _name, _data} = event), do: event
  defp normalize_event({name, data}), do: {:event, name, data}
  defp normalize_event(name) when is_binary(name), do: {:event, name, nil}

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

  def handle_call({:event, name, data}, _from, interpreter) do
    interpreter = Interpreter.send_event(interpreter, {name, data})
    {:reply, interpreter.state, interpreter}
  end

  @impl true
  def handle_cast({:event, _name, _data} = event, interpreter) do
    {:noreply, interpreter, {:continue, event}}
  end

  def handle_cast(@protean_terminate, interpreter) do
    {:stop, :normal, interpreter}
  end

  @impl true
  def handle_continue({:event, name, data}, interpreter) do
    {:noreply, Interpreter.send_event(interpreter, {name, data})}
  end

  @impl true
  def terminate(:normal, interpreter) do
    Interpreter.stop(interpreter)
    :ok
  end
end
