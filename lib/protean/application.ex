defmodule Protean.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    Supervisor.start_link(Protean.ProcessManager, strategy: :one_for_one)
  end
end
