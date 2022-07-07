defmodule Protean.Machine do
  @moduledoc """
  `Protean.Machine` is the purely-functional core of Protean, responsible
  for transitioning a machine from one state to the next as a result of an
  event.
  """

  alias __MODULE__
  alias Protean.{State, StateNode, MachineConfig}

  @enforce_keys [:root]
  defstruct [:root]

  @typedoc """
  A full Protean machine configuration.
  """
  @type t :: %Machine{
          root: StateNode.t()
        }

  @typedoc """
  An event name used in a machine configuration and when sending events to a
  machine.
  """
  @type event_name :: String.t()

  @typedoc """
  An event that can be sent to a machine to trigger a transition.
  """
  @type event :: {event_name, term}

  def new(config) do
    %Machine{
      root: MachineConfig.parse!(config)
    }
  end

  @doc """
  Returns the initial `Protean.State` for a given machine.
  """
  @spec initial_state(Machine.t()) :: State.t()
  def initial_state(%Machine{root: root}) do
    %State{value: root.id}
  end

  @doc """
  Given a machine, a machine state, and an event, transition to the next state
  if the machine defines a transition for the given state and event.
  """
  @spec transition(Machine.t(), State.t(), event) :: State.t()
  def transition(_machine, state, _event) do
    state
  end
end
