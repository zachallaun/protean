defmodule Protean.Application do
  @moduledoc false
  use Application

  @config_keys [:registry, :supervisor]

  def start(_type, _args) do
    configure!()

    children = [
      {supervisor(), name: Protean.Supervisor, strategy: :one_for_one},
      {registry(), name: Protean.Registry, keys: :unique}
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  def registry do
    :persistent_term.get({__MODULE__, :registry})
  end

  def supervisor do
    :persistent_term.get({__MODULE__, :supervisor})
  end

  defp configure! do
    for key <- @config_keys do
      :persistent_term.put(
        {__MODULE__, key},
        Application.fetch_env!(:protean, key)
      )
    end
  end
end
