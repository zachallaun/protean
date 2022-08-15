defmodule Trigger do
  @moduledoc """
  Simple GenServer-based mechanism for waiting on another process.

  Each value can only be triggered once. Intended to be restarted before each test. Subsequent
  calls to `await` will immediately return with `{:error, :already_triggered}`.

  ## Usage

      # In some async process
      :timer.sleep(1000)
      Trigger.trigger(:after_sleep)

      # In the test
      :ok = Trigger.await(:after_sleep)
      assert ...
      {:error, :already_triggered} = Trigger.await(:after_sleep)

  """

  use GenServer

  def start_link(name: name) do
    GenServer.start_link(__MODULE__, :ok, name: name)
  end

  @spec trigger(GenServer.server(), term()) :: :ok
  def trigger(server, value) do
    GenServer.call(server, {:trigger, value})
  end

  @spec triggered?(GenServer.server(), term()) :: boolean()
  def triggered?(server, value) do
    GenServer.call(server, {:triggered?, value})
  end

  @spec await(GenServer.server(), term(), timeout()) :: :ok | :triggered
  def await(server, value, timeout \\ 5000) do
    GenServer.call(server, {:await, value}, timeout)
  end

  # Protean Action helpers
  @behaviour Protean.Action

  def action(name, value) do
    Protean.Action.new(__MODULE__, {:trigger, name, value})
  end

  @impl true
  def exec_action({:trigger, name, value}, interpreter) do
    trigger(name, value)
    {:cont, interpreter}
  end

  # Server callbacks

  @impl true
  def init(:ok) do
    {:ok, {Map.new(), MapSet.new()}}
  end

  @impl true
  def handle_call({:triggered?, value}, _from, {waiting, triggered}) do
    {:reply, value in triggered, {waiting, triggered}}
  end

  def handle_call({:await, value}, from, {waiting, triggered}) do
    if value in triggered do
      {:reply, {:error, :already_triggered}, {waiting, triggered}}
    else
      waiting = Map.update(waiting, value, [from], &[from | &1])
      {:noreply, {waiting, triggered}}
    end
  end

  def handle_call({:trigger, value}, _from, {waiting, triggered}) do
    {interested, waiting} = Map.pop(waiting, value, [])
    for pid <- interested, do: GenServer.reply(pid, :ok)
    {:reply, :ok, {waiting, MapSet.put(triggered, value)}}
  end
end
