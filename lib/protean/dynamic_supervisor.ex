defmodule Protean.DynamicSupervisor do
  @moduledoc false

  use DynamicSupervisor

  require Logger

  def start_link(init_arg) do
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  def start_child(spec) do
    DynamicSupervisor.start_child(__MODULE__, spec)
  catch
    :exit, {:noproc, _} -> error_noproc!()
  end

  def terminate_child(pid) do
    DynamicSupervisor.terminate_child(__MODULE__, pid)
  catch
    :exit, {:noproc, _} -> error_noproc!()
  end

  @impl true
  def init(_init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  defp error_noproc! do
    require Logger

    Logger.error("""
    Child processes started with `:invoke` require that `Protean` has been started. Are you sure \
    that `Protean` has been started under your supervision tree?\
    """)
  end
end
