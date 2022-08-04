defmodule Protean.Events do
  @moduledoc false

  defmodule Platform do
    defstruct [:type, :id]
  end

  @doc "Create a platform event."
  def platform(:init) do
    %Platform{type: :init}
  end

  def platform(:done, id) do
    %Platform{type: :done, id: id}
  end

  def platform(:after, id) do
    %Platform{type: :after, id: id}
  end

  def platform(:invoke, subtype, id) when subtype in [:done, :error] do
    %Platform{type: {:invoke, subtype}, id: id}
  end
end
