defmodule Protean.ProcessManager do
  @moduledoc false

  use Supervisor

  @compile {:inline, supervisor: 0, registry: 0, subprocess_key: 1}

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
    {reg_module, reg_name} = registry()

    reg_module.select(reg_name, [
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
    {reg_module, reg_name} = registry()

    case reg_module.lookup(reg_name, subprocess_key(id)) do
      [{_, proc}] -> {:ok, proc}
      [] -> :error
    end
  end

  @doc """
  Look up the subprocess of the calling process by its monitor `ref`.
  """
  @spec subprocess_by_ref(reference()) :: {:ok, {id :: term(), subprocess}} | nil
  def subprocess_by_ref(ref) do
    {reg_module, reg_name} = registry()

    reg_module.select(reg_name, [
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
    {reg_module, reg_name} = registry()
    {:via, reg_module, {reg_name, name}}
  end

  @doc false
  def register_subprocess(id, value) do
    {reg_module, reg_name} = registry()
    reg_module.register(reg_name, subprocess_key(id), value)
  end

  @doc false
  def unregister_subprocess(id) do
    {reg_module, reg_name} = registry()
    reg_module.unregister(reg_name, subprocess_key(id))
  end

  @doc false
  def start_child(child_spec) do
    {sup_module, sup_name} = supervisor()
    sup_module.start_child(sup_name, child_spec)
  end

  @doc false
  def terminate_child(pid) do
    {sup_module, sup_name} = supervisor()
    sup_module.terminate_child(sup_name, pid)
  end

  @doc false
  def which_children do
    {sup_module, sup_name} = supervisor()
    sup_module.which_children(sup_name)
  end

  @doc false
  def count_registered do
    {reg_module, reg_name} = registry()
    reg_module.count(reg_name)
  end

  defp subprocess_key(id) do
    {:subprocess, self(), id}
  end

  @impl true
  def init(_arg) do
    configure!()

    {sup_module, sup_name} = supervisor()
    {reg_module, reg_name} = registry()

    children = [
      {sup_module, name: sup_name, strategy: :one_for_one},
      {reg_module, name: reg_name, keys: :unique}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  defp supervisor do
    :persistent_term.get({__MODULE__, :supervisor})
  end

  defp registry do
    :persistent_term.get({__MODULE__, :registry})
  end

  defp configure! do
    :persistent_term.put(
      {__MODULE__, :supervisor},
      {Application.fetch_env!(:protean, :supervisor), Module.concat(__MODULE__, Supervisor)}
    )

    :persistent_term.put(
      {__MODULE__, :registry},
      {Application.fetch_env!(:protean, :registry), Module.concat(__MODULE__, Registry)}
    )
  end
end
