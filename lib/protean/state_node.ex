defmodule Protean.StateNode do
  @moduledoc false

  alias __MODULE__
  alias Protean.{Transition}

  defstruct [:id, :type, :transitions, :states, :initial]

  @type atomic() :: %StateNode{
          id: atom(),
          type: :atomic,
          transitions: [Transition.t()],
          states: nil,
          initial: nil
        }

  @type final() :: %StateNode{
          id: atom(),
          type: :final,
          transitions: nil,
          states: nil,
          initial: nil
        }

  @type compound() :: %StateNode{
          id: atom(),
          type: :compound,
          transitions: [Transition.t()],
          states: [StateNode.t()],
          initial: atom()
        }

  @type t() :: atomic() | final() | compound()

  def from_config(id, config), do: from_config(config[:type], id, config)

  def from_config(:atomic, id, config) do
    %StateNode{
      type: :atomic,
      id: id,
      transitions: transitions_from_config(config)
    }
  end

  def from_config(:final, id, _config) do
    %StateNode{
      type: :final,
      id: id
    }
  end

  def from_config(:compound, id, config) do
    states =
      for {state_id, state_config} <- Keyword.fetch!(config, :states) do
        StateNode.from_config(state_id, state_config)
      end

    %StateNode{
      type: :compound,
      id: id,
      transitions: transitions_from_config(config),
      initial: Keyword.fetch!(config, :initial),
      states: states
    }
  end

  defp transitions_from_config(config) do
    for {event_name, transition_config} <- Keyword.get(config, :on, []) do
      event_name
      |> to_string()
      |> Transition.from_config(transition_config)
    end
  end
end
