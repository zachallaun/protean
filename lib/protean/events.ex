defmodule Protean.Events do
  @moduledoc false

  defmodule Platform do
    @moduledoc false

    defstruct [:type, :id, :payload]
  end

  @doc "Create a platform event."
  def platform(:init) do
    %Platform{type: :init}
  end

  def platform(:done, id) do
    %Platform{type: :done, id: id}
  end

  def platform(:spawn, subtype, id) when subtype in [:done, :error] do
    %Platform{type: {:spawn, subtype}, id: id}
  end

  @doc "Add a payload to a platform event."
  def with_payload(%Platform{} = event, payload) do
    %{event | payload: payload}
  end
end
