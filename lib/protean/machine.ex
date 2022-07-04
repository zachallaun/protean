defmodule Protean.Machine do
  @moduledoc """
  `Protean.Machine` is the purely-functional core of Protean, responsible
  for transitioning a machine from one state to the next as a result of an
  event.
  """

  alias __MODULE__
  alias Protean.State

  @enforce_keys [:config, :initial_state, :handler]
  defstruct [:config, :initial_state, :handler]

  @typedoc """
  A full Protean machine configuration.
  """
  @type t() :: %Machine{
          config: list(term()),
          initial_state: atom(),
          handler: term()
        }

  @typedoc """
  An event name used in a machine configuration and when sending events to a
  machine.
  """
  @type event_name() :: String.t()

  @typedoc """
  An event that can be sent to a machine to trigger a transition.
  """
  @type event() :: {event_name(), term()}

  def new(module) when is_atom(module) do
    with config <- module.protean_config(),
         initial when is_atom(initial) <- Keyword.get(config, :initial) do
      %Machine{initial_state: %State{value: initial}, config: config, handler: nil}
    else
      _ -> raise("Need an :initial state pls")
    end
  end

  @doc """
  Given a machine, a machine state, and an event, transition to the next state
  if the machine defines a transition for the given state and event.
  """
  @spec transition(Machine.t(), State.t(), String.t()) :: State.t()
  def transition(machine, state, event) do
    states = Keyword.get(machine.config, :states)
    node = Keyword.get(states, state.value)
    transitions = Keyword.get(node, :on)
    next = Keyword.get(transitions, event)

    if is_nil(next) do
      state
    else
      %State{value: next, event: event}
    end
  end
end
