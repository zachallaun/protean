defmodule Protean.Action.Invoke do
  @moduledoc false

  def task(id, thing), do: %{id: id, thing: thing}

  def task_cancel(id), do: %{id: id}
end
