defmodule Protean.TestCase do
  @moduledoc """
  Utilities for testing modules defined with `use Protean`.
  """

  use ExUnit.CaseTemplate

  setup context do
    if server = context[:machine] do
      setup_server(server)
    end
  end

  defp setup_server({module, opts}) do
    {:ok, pid} = module.start_link(opts)
    ref = Process.monitor(pid)

    on_exit(fn -> Process.exit(pid, :normal) end)

    [machine: pid, ref: ref]
  end

  defp setup_server(module), do: setup_server({module, []})
end
