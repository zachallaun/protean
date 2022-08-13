defmodule Protean.ProcessManager do
  @moduledoc false

  use Supervisor

  @compile {:inline, supervisor: 0, registry: 0}

  @doc """
  Start a child process under the Protean supervisor.
  """
  def start_child(child_spec) do
    {sup_module, sup_name} = supervisor()
    sup_module.start_child(sup_name, child_spec)
  end

  @doc """
  Terminate a child process under the Protean supervisor.
  """
  def terminate_child(id) do
    {sup_module, sup_name} = supervisor()
    sup_module.terminate_child(sup_name, id)
  end

  @doc """
  Generate a process name using the Protean registry.
  """
  def via_registry(id) do
    {reg_module, reg_name} = registry()
    {:via, reg_module, {reg_name, id}}
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
