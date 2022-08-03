defmodule Protean.Supervisor do
  @moduledoc false

  use DynamicSupervisor

  def start_link(opts \\ []) do
    DynamicSupervisor.start_link(__MODULE__, [], Keyword.merge([name: __MODULE__], opts))
  end

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

  @impl true
  def init(_init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  defp error_noproc!(error) do
    require Logger

    Logger.error("""
    child processes started with `:invoke` require that `Protean.Supervisor` has been started.
    Are you sure that it is included under your supervision tree?
    """)

    exit(error)
  end
end
