defmodule Protean.ProcessManager do
  @moduledoc false

  use Supervisor

  @compile {:inline, supervisor: 0, registry: 0, subprocess_key: 1}

  @supervisor Module.concat(__MODULE__, Supervisor)
  @registry Module.concat(__MODULE__, Registry)

  @type subprocess :: {pid(), reference(), meta :: term()}

  @doc """
  Start a registered and monitored subprocess of the calling process.

  The subprocess will be saved as a `{pid(), ref, meta}` tuple.
  """
  @spec start_subprocess(term(), Supervisor.child_spec(), term()) :: :ok | {:error, term()}
  def start_subprocess(id, child_spec, meta \\ nil) do
    with :error <- fetch_subprocess(id),
         {:ok, pid} <- start_child(child_spec),
         ref <- Process.monitor(pid),
         {:ok, _} <- register_subprocess(id, {pid, ref, meta}) do
      :ok
    else
      {:ok, {pid, _ref, _meta}} ->
        {:error, {:already_started, pid}}

      {:error, {:already_started, pid}} ->
        terminate_child(pid)

        require Logger

        Logger.warn(
          "possible race condition occurred; terminated process #{pid} should not have started"
        )

        {:error, {:already_started, pid}}

      other ->
        other
    end
  end

  @doc """
  Stop and deregister a subprocess of the calling process.
  """
  @spec stop_subprocess(term()) :: :ok
  def stop_subprocess(id) do
    case fetch_subprocess(id) do
      {:ok, proc} -> stop_subprocesses([{id, proc}])
      :error -> :ok
    end
  end

  @doc """
  Stop and deregister all subprocesses of the calling process.
  """
  @spec stop_all_subprocesses() :: :ok
  def stop_all_subprocesses do
    stop_subprocesses(subprocesses())
  end

  defp stop_subprocesses(subprocesses) do
    for {id, {pid, ref, _}} <- subprocesses do
      Process.demonitor(ref, [:flush])
      terminate_child(pid)
      unregister_subprocess(id)
    end

    :ok
  end

  @doc """
  Select all registered subprocesses of the calling process.
  """
  def subprocesses do
    registry().select(@registry, [
      {
        {subprocess_key(:"$1"), self(), :"$2"},
        [],
        [{{:"$1", :"$2"}}]
      }
    ])
  end

  @doc """
  Look up the subprocess of the calling process registered by `id`.
  """
  @spec fetch_subprocess(term()) :: {:ok, subprocess} | :error
  def fetch_subprocess(id) do
    case registry().lookup(@registry, subprocess_key(id)) do
      [{_, proc}] -> {:ok, proc}
      [] -> :error
    end
  end

  @doc """
  Look up the subprocess of the calling process by its monitor `ref`.
  """
  @spec subprocess_by_ref(reference()) :: {:ok, {id :: term(), subprocess}} | nil
  def subprocess_by_ref(ref) do
    registry().select(@registry, [
      {
        {subprocess_key(:"$1"), self(), {:"$2", ref, :"$3"}},
        [],
        [{{:"$1", :"$2", :"$3"}}]
      }
    ])
    |> case do
      [{id, pid, meta}] -> {:ok, {id, {pid, ref, meta}}}
      _ -> nil
    end
  end

  @doc false
  def via_registry(name) do
    {:via, registry(), {@registry, name}}
  end

  @doc false
  def register_subprocess(id, value) do
    registry().register(@registry, subprocess_key(id), value)
  end

  @doc false
  def unregister_subprocess(id) do
    registry().unregister(@registry, subprocess_key(id))
  end

  @doc false
  def start_child(child_spec) do
    supervisor().start_child(@supervisor, child_spec)
  end

  @doc false
  def terminate_child(pid) do
    supervisor().terminate_child(@supervisor, pid)
  end

  @doc false
  def which_children do
    supervisor().which_children(@supervisor)
  end

  @doc false
  def count_registered do
    registry().count(@registry)
  end

  defp subprocess_key(id) do
    {:subprocess, self(), id}
  end

  # Supervisor

  def start_link(_) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    configure!()

    children = [
      {supervisor(), name: @supervisor, strategy: :one_for_one},
      {registry(), name: @registry, keys: :unique}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  defp supervisor do
    :persistent_term.get(@supervisor)
  end

  defp registry do
    :persistent_term.get(@registry)
  end

  defp configure! do
    :persistent_term.put(
      @supervisor,
      Application.get_env(:protean, :supervisor, DynamicSupervisor)
    )

    :persistent_term.put(
      @registry,
      Application.get_env(:protean, :registry, Registry)
    )
  end
end
