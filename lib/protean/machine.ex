defmodule Protean.Machine do
  @moduledoc """
  `Protean.Machine` is the purely-functional core of Protean, responsible
  for transitioning a machine from one state to the next as a result of an
  event.
  """

  alias __MODULE__
  alias Protean.{State, StateNode, MachineConfig, Transition, Utilities}

  defstruct [
    :root,
    idmap: %{}
  ]

  @typedoc """
  A full Protean machine configuration.
  """
  @type t :: %Machine{
          root: StateNode.t(),
          idmap: %{StateNode.id() => StateNode.t()}
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
    root = MachineConfig.parse!(config)
    idmap = Utilities.Tree.tree_reduce(root, &idmap_reducer/2, %{})

    %Machine{
      root: root,
      idmap: idmap
    }
  end

  defp idmap_reducer(node, idmap) do
    {Map.put(idmap, node.id, node), node.states}
  end

  @doc """
  Returns the initial `Protean.State` for a given machine.
  """
  @spec initial_state(Machine.t()) :: State.t()
  def initial_state(%Machine{root: root}) do
    active_ids =
      root
      |> StateNode.resolve_to_leaves()
      |> Enum.map(& &1.id)

    %State{value: active_ids}
  end

  @doc """
  Given a machine, a machine state, and an event, transition to the next state
  if the machine defines a transition for the given state and event.
  """
  @spec transition(Machine.t(), State.t(), event) :: State.t()
  def transition(machine, state, event) do
    enabled_transition =
      machine
      |> active_nodes(state)
      |> first_enabled_transition(event)

    if enabled_transition do
      resolved_target_ids =
        enabled_transition
        |> Transition.target()
        |> lookup_by_id(machine)
        |> StateNode.resolve_to_leaves()
        |> Enum.map(& &1.id)

      %State{value: resolved_target_ids, event: event}
    else
      state
    end
  end

  defp lookup_by_id(id, machine), do: machine.idmap[id]

  @spec first_enabled_transition([StateNode.t()], event) :: Transition.t() | nil
  defp first_enabled_transition([], _event), do: nil

  defp first_enabled_transition([node | rest], event) do
    case StateNode.enabled_transition(node, event) do
      nil -> first_enabled_transition(rest, event)
      transition -> transition
    end
  end

  @spec active_nodes(Machine.t(), State.t()) :: [StateNode.t()]
  defp active_nodes(%Machine{idmap: idmap}, %State{value: value}) do
    value
    |> Enum.flat_map(&StateNode.ancestor_ids/1)
    |> Enum.uniq()
    |> Enum.map(&idmap[&1])
  end
end
