defmodule Protean.Transition do
  @moduledoc false

  alias __MODULE__
  alias Protean.{Machine, State, StateNode, Action}

  defstruct [:config, :event, :source, :actions, :type]

  @type t() :: %Transition{
          config: transition_config(),
          event: Machine.event(),
          source: StateNode.t(),
          actions: [Action.t()],
          type: :internal | :external
        }

  @type transition_config() :: [target: State.state_value()]

  @spec target(Transition.t()) :: State.state_value()
  def target(%Transition{config: config}), do: config[:target]
end
