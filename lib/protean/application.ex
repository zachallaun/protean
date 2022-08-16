defmodule Protean.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    children = [
      Protean.ProcessManager,
      Protean.PubSub
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
