defmodule Protean.Machine do
  @moduledoc """
  `Protean.Machine` is the purely-functional core of Protean, responsible
  for transitioning a machine from one state to the next as a result of an
  event.
  """

  alias __MODULE__
  alias Protean.MachineConfig
  alias Protean.State
  alias Protean.StateNode
  alias Protean.Transition
  alias Protean.Utilities

  defstruct [
    :root,
    :handler,
    idmap: %{},
    initial_context: %{}
  ]

  @typedoc "A full Protean machine configuration."
  @type t :: %Machine{
          root: StateNode.t(),
          handler: module,
          idmap: %{StateNode.id() => StateNode.t()},
          initial_context: State.context()
        }

  @typedoc "An event name used in a machine configuration and when sending events to a machine."
  @type event_name :: String.t()

  @typedoc "Data payload sent along with an event."
  @type event_data :: term()

  @typedoc """
  The full representation of an event. `t:sendable_event()` is normalized to this form.
  """
  @type event :: {:event, event_name(), event_data()}

  @typedoc "An event that can be sent to a machine to trigger a transition."
  @type sendable_event :: event() | event_name() | {event_name(), event_data()}

  def new(config, opts \\ []) do
    {root, context} = MachineConfig.parse!(config)
    idmap = Utilities.Tree.tree_reduce(root, &idmap_reducer/2, %{})

    %Machine{
      root: root,
      idmap: idmap,
      initial_context: context,
      handler: Keyword.get(opts, :handler)
    }
  end

  defp idmap_reducer(node, idmap) do
    {Map.put(idmap, node.id, node), node.states}
  end

  @doc """
  Returns the initial `Protean.State` for a given machine.
  """
  @spec initial_state(Machine.t()) :: State.t()
  def initial_state(%Machine{root: root, initial_context: context} = machine) do
    active_ids =
      root
      |> StateNode.resolve_to_leaves()
      |> Enum.map(& &1.id)

    entry_ids =
      active_ids
      |> Enum.flat_map(&StateNode.ancestor_ids/1)
      |> Enum.uniq()

    State.new(active_ids)
    |> State.assign(context)
    |> State.assign_actions(entry_actions(machine, entry_ids))
  end

  @spec take_transitions(Machine.t(), State.t(), [Transition.t()]) :: State.t()
  def take_transitions(machine, state, transitions)

  def take_transitions(_machine, state, []), do: state

  def take_transitions(machine, state, transitions) do
    Enum.reduce(transitions, state, fn transition, state ->
      apply_transition(machine, state, transition)
    end)
  end

  defp apply_transition(machine, %State{value: value} = state, transition) do
    actions = State.actions(state)

    exit_ids =
      Enum.flat_map(value, &StateNode.ancestor_ids/1)
      |> Enum.uniq()
      |> Enum.filter(&will_exit?(machine, &1, transition))

    # TODO: this is wrong, it needs to be the full set of states that will
    # be entered and not just leaves
    entry_ids =
      case Transition.target(transition) do
        nil ->
          []

        target ->
          target
          |> lookup_by_id(machine)
          |> StateNode.resolve_to_leaves()
          |> Enum.map(& &1.id)
      end

    new_actions =
      Enum.concat([
        exit_actions(machine, exit_ids),
        List.wrap(transition.actions),
        entry_actions(machine, entry_ids)
      ])

    value =
      (value -- exit_ids)
      |> Enum.concat(entry_ids)
      |> Enum.uniq()

    state
    |> Map.put(:value, value)
    |> State.assign_actions(List.wrap(actions) ++ new_actions)
  end

  @spec will_exit?(Machine.t(), StateNode.id(), Transition.t()) :: boolean
  defp will_exit?(machine, id, transition) do
    transition
    |> Transition.targets()
    |> Enum.any?(fn target_id ->
      target_is_descendant? = StateNode.descendant?(target_id, id)
      parallel_ancestor? = common_ancestor(machine, id, target_id).type == :parallel

      !target_is_descendant? && !parallel_ancestor?
    end)
  end

  defp common_ancestor(%Machine{idmap: idmap}, id1, id2) do
    ancestor_id = StateNode.common_ancestor_id(id1, id2)
    Map.fetch!(idmap, ancestor_id)
  end

  @spec select_automatic_transitions(Machine.t(), State.t()) :: [Transition.t()]
  def select_automatic_transitions(machine, state) do
    nodes = active_nodes(machine, state)

    case first_enabled_transition(nodes, machine, state, state.event, :automatic_transitions) do
      nil -> []
      transition -> [transition]
    end
  end

  @spec select_transitions(Machine.t(), State.t(), event()) :: [Transition.t()]
  def select_transitions(machine, state, event) do
    # TODO: Handle conflicting transitions
    # TODO: order nodes correctly (specificity + document order)
    nodes = active_nodes(machine, state)

    case first_enabled_transition(nodes, machine, state, event) do
      nil -> []
      transition -> [transition]
    end
  end

  @doc """
  Given a machine, a machine state, and an event, transition to the next state
  if the machine defines a transition for the given state and event.
  """
  @spec transition(Machine.t(), State.t(), sendable_event()) :: State.t()
  def transition(machine, state, event) do
    with event <- normalize_event(event),
         transitions <- select_transitions(machine, state, event) do
      take_transitions(machine, state, transitions)
    end
  end

  @doc """
  Normalizes any valid `t:sendable_event()` to a `t:event()`. Event data will default to `nil` if
  not provided with the event.
  """
  @spec normalize_event(sendable_event()) :: event()
  def normalize_event({:event, _name, _data} = event), do: event
  def normalize_event({name, data}), do: {:event, name, data}
  def normalize_event(name) when is_binary(name), do: {:event, name, nil}

  defp lookup_by_id(id, machine), do: machine.idmap[id]

  defp first_enabled_transition(nodes, machine, state, event, attribute \\ :transitions) do
    nodes
    |> Enum.flat_map(&Map.get(&1, attribute))
    |> find_enabled_transition(machine, state, event)
  end

  defp find_enabled_transition(transitions, machine, state, event) do
    Enum.find(transitions, fn transition ->
      Transition.enabled?(transition, event, state, machine.handler)
    end)
  end

  @spec active_nodes(Machine.t(), State.t()) :: [StateNode.t()]
  defp active_nodes(machine, %State{value: value}) do
    value
    |> Enum.flat_map(&ancestors(machine, &1))
    |> Enum.uniq()
  end

  @spec ancestors(Machine.t(), StateNode.id()) :: [StateNode.t()]
  defp ancestors(%Machine{idmap: idmap}, id) do
    id
    |> StateNode.ancestor_ids()
    |> Enum.map(&idmap[&1])
  end

  # @spec entry_order(Machine.t(), [StateNode.id()]) :: [StateNode.t()]
  # defp entry_order(machine, ids) do
  #   ids
  #   |> Enum.flat_map(&ordered_nodes(machine, &1, :desc))
  #   |> Enum.uniq()
  # end

  # @spec exit_order(Machine.t(), [StateNode.id()]) :: [StateNode.t()]
  # defp exit_order(machine, ids) do
  #   ids
  #   |> Enum.flat_map(&ordered_nodes(machine, &1, :asc))
  #   |> Enum.uniq()
  # end

  # @spec ordered_nodes(Machine.t(), StateNode.id(), :asc | :desc) :: [StateNode.t()]
  # defp ordered_nodes(machine, id, order)

  # defp ordered_nodes(machine, id, :desc) do
  #   ordered_nodes(machine, id, :asc) |> Enum.reverse()
  # end

  # defp ordered_nodes(machine, [_ | parent] = id, :asc) do
  #   [machine.idmap[id] | ordered_nodes(machine, parent, :asc)]
  # end

  # defp ordered_nodes(_machine, [], :asc), do: []

  @spec entry_actions(Machine.t(), [StateNode.id()]) :: [Action.t()]
  defp entry_actions(machine, ids) do
    machine
    |> entry_order(ids)
    |> Enum.flat_map(& &1.entry)
  end

  @spec exit_actions(Machine.t(), [StateNode.id()]) :: [Action.t()]
  defp exit_actions(machine, ids) do
    machine
    |> exit_order(ids)
    |> Enum.flat_map(& &1.exit)
  end

  defp entry_order(machine, ids), do: document_order(machine, ids)

  defp exit_order(machine, ids), do: document_order(machine, ids, :desc)

  defp document_order(machine, ids, sorter \\ :asc) do
    machine
    |> lookup_nodes(ids)
    |> Enum.sort_by(& &1.order, sorter)
  end

  defp lookup_nodes(machine, ids) do
    Enum.map(ids, fn id -> machine.idmap[id] end)
  end
end
