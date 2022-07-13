defmodule Protean.State do
  @moduledoc false

  alias __MODULE__
  alias Protean.{Machine, StateNode, Action}

  defstruct [
    :value,
    :event,
    context: %{},
    actions: []
  ]

  @type t :: %State{
          value: value,
          event: Machine.event() | nil,
          context: Machine.context(),
          actions: [Action.t()]
        }

  @type value :: [StateNode.id()]
end
