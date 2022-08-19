defmodule Protean.ProcessManager do
  @moduledoc false

  use Supervisor

  @compile {:inline, supervisor: 0, task_supervisor: 0, registry: 0, subprocess_key: 1}

  @supervisor Module.concat(__MODULE__, Supervisor)
  @task_supervisor Module.concat(__MODULE__, TaskSupervisor)
  @registry Module.concat(__MODULE__, Registry)

  @type subprocess :: {id :: term(), pid(), reference(), meta :: term()}

  @doc """
  Start a registered and monitored subprocess of the calling process.

  The subprocess will be saved as a `{id, pid, ref, meta}` tuple.
  """
  @spec start_subprocess(term(), Supervisor.child_spec(), term()) :: :ok | {:error, term()}
  def start_subprocess(id, child_spec, meta \\ nil) do
    with :error <- fetch_subprocess(id),
         {:ok, pid} <- start_child(child_spec),
         ref <- Process.monitor(pid),
         {:ok, _} <- register_subprocess({id, pid, ref, meta}) do
      :ok
    else
      {:ok, {_id, pid, _ref, _meta}} ->
        {:error, {:already_started, pid}}

      {:error, {:already_started, pid}} ->
        supervisor().terminate_child(@supervisor, pid)

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
  Start a registered async task. See `Task.Supervisor.async_nolink/3`.


  """
  @spec start_task(term(), function() | mfa()) :: :ok
  def start_task(id, task) do
    with task <- async_nolink(task),
         {:ok, _} <- register_subprocess({id, task.pid, task.ref, task: true}) do
      :ok
    end
  end

  @doc """
  Stop and deregister a subprocess of the calling process.
  """
  @spec stop_subprocess(term()) :: :ok
  def stop_subprocess(id) do
    case fetch_subprocess(id) do
      {:ok, proc} -> stop_subprocesses([proc])
      :error -> :ok
    end
  end

  @doc """
  Stop and deregister all subprocesses of the calling process.
  """
  @spec stop_all_subprocesses() :: :ok
  def stop_all_subprocesses do
    stop_subprocesses(registered_subprocesses())
  end

  defp stop_subprocesses(subprocesses) do
    for {id, pid, ref, meta} <- subprocesses do
      if Keyword.get(meta, :task, false) do
        task_supervisor().terminate_child(@task_supervisor, pid)
      else
        supervisor().terminate_child(@supervisor, pid)
      end

      Process.demonitor(ref, [:flush])
      unregister_subprocess(id)
    end

    :ok
  end

  @doc """
  Select all registered subprocesses of the calling process.
  """
  def registered_subprocesses do
    registry().select(@registry, [
      {
        {subprocess_key(:_), self(), :"$1"},
        [],
        [:"$1"]
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
  @spec subprocess_by_ref(reference()) :: subprocess | nil
  def subprocess_by_ref(ref) do
    registry().select(@registry, [
      {
        {subprocess_key(:_), self(), {:"$1", :"$2", ref, :"$3"}},
        [],
        [{{:"$1", :"$2", :"$3"}}]
      }
    ])
    |> case do
      [{id, pid, meta}] -> {id, pid, ref, meta}
      _ -> nil
    end
  end

  def whereis(id) do
    case GenServer.whereis(via_registry(id)) do
      nil -> :error
      pid -> {:ok, pid}
    end
  end

  def via_registry(id) do
    {:via, registry(), {@registry, id}}
  end

  def register_subprocess({id, _, _, _} = value) do
    registry().register(@registry, subprocess_key(id), value)
  end

  def unregister_subprocess(id) do
    registry().unregister(@registry, subprocess_key(id))
  end

  def start_child(child_spec) do
    supervisor().start_child(@supervisor, child_spec)
  end

  def async_nolink(fun) when is_function(fun) do
    task_supervisor().async_nolink(@task_supervisor, fun)
  end

  def async_nolink({mod, fun, args}) when is_atom(mod) and is_atom(fun) and is_list(args) do
    task_supervisor().async_nolink(@task_supervisor, mod, fun, args)
  end

  def which_children do
    supervisor().which_children(@supervisor)
  end

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
      {task_supervisor(), name: @task_supervisor},
      {registry(), name: @registry, keys: :unique}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  defp task_supervisor, do: Task.Supervisor
  defp supervisor, do: :persistent_term.get(@supervisor)
  defp registry, do: :persistent_term.get(@registry)

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
