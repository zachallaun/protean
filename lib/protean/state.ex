defmodule Protean.State do
  @moduledoc false

  alias __MODULE__
  alias Protean.{StateNode, Action}

  defstruct [
    :value,
    :event,
    actions: []
  ]

  @type t :: %State{
          value: [StateNode.id()],
          event: {String.t(), term} | nil,
          actions: [Action.t()]
        }
end
