defmodule Protean.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    children = [
      supervisor(name: Protean.Supervisor, strategy: :one_for_one)
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  defp supervisor(overrides) do
    Application.fetch_env!(:protean, :machine_supervisor)
    |> with_overrides(overrides)
  end

  defp with_overrides(module, overrides) when is_atom(module) do
    {module, overrides}
  end

  defp with_overrides({module, defaults}, overrides) do
    {module, Keyword.merge(defaults, overrides)}
  end
end
