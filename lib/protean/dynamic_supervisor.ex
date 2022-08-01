defmodule Protean.DynamicSupervisor do
  @moduledoc false

  use DynamicSupervisor

  # Client

  def start_child(supervisor, spec) do
    DynamicSupervisor.start_child(supervisor, spec)
  catch
    :exit, {:noproc, _} = error -> error_noproc!(error)
  end

  def terminate_child(supervisor, pid) do
    DynamicSupervisor.terminate_child(supervisor, pid)
  catch
    :exit, {:noproc, _} = error -> error_noproc!(error)
  end

  # Callbacks

  @impl true
  def init(_init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  defp error_noproc!(error) do
    require Logger

    Logger.error("""
    Child processes started with `:invoke` require that `Protean` has been started. Are you sure \
    that `Protean` has been started under your supervision tree?\
    """)

    exit(error)
  end
end
