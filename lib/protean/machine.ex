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
  @type event_data :: term

  @typedoc """
  The full representation of an event. `t:sendable_event()` is normalized to this form.
  """
  @type event :: {event_name, event_data}

  @typedoc "An event that can be sent to a machine to trigger a transition."
  @type sendable_event :: event | event_name

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
  @spec initial_state(t) :: State.t()
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

  @spec take_transitions(t, State.t(), [Transition.t()]) :: State.t()
  def take_transitions(machine, state, transitions)

  def take_transitions(_machine, state, []), do: state

  def take_transitions(machine, state, transitions) do
    Enum.reduce(transitions, state, fn transition, state ->
      apply_transition(machine, state, transition)
    end)
  end

  # This function is fundamentally wrong, we need to return {transition, entry_set, exit_set}
  # or something so that we can ensure that none of the transitions conflict with each other
  # (they should have non-overlapping exit sets, otherwise we have to figure out which one "wins")
  defp apply_transition(machine, state, transition) do
    %{idmap: idmap} = machine
    active = active_nodes(machine, state)
    targets = effective_targets(transition.targets, idmap)
    domain_id = transition_domain(transition, targets)

    to_exit =
      domain_id
      |> exit_set(active)
      |> Enum.sort_by(& &1.order, :desc)

    to_enter =
      domain_id
      |> entry_set(targets, idmap)
      |> Enum.sort_by(& &1.order, :asc)

    value =
      (state.value -- Enum.map(to_exit, & &1.id)) ++
        Enum.map(targets, & &1.id)

    new_active = active_nodes(machine, value)

    # if internal, we don't exit states we normally would have exited if they're a part of the
    # new active set
    to_exit =
      if transition.internal do
        to_exit -- new_active
      else
        to_exit
      end

    # if internal, we don't enter states we normally would if they were already active
    to_enter =
      if transition.internal do
        to_enter -- active
      else
        to_enter
      end

    actions =
      Enum.flat_map(to_exit, & &1.exit) ++
        transition.actions ++
        Enum.flat_map(to_enter, & &1.entry)

    state
    |> Map.put(:value, value)
    |> State.put_actions(actions)
  end

  defp entry_set(domain_id, targets, idmap)

  defp entry_set([], targets, idmap),
    do: entry_set(["#"], targets, idmap)

  defp entry_set(_, [], _), do: []

  defp entry_set(domain_id, targets, idmap) do
    case idmap[domain_id] do
      %{type: :atomic} ->
        []

      %{type: :final} ->
        []

      %{type: :compound} = compound ->
        child =
          Enum.find(compound.states, fn child ->
            Enum.any?(targets, &loose_descendant?(&1.id, child.id))
          end)

        if child do
          [child | entry_set(child.id, targets, idmap)]
        else
          []
        end

      %{type: :parallel} = parallel ->
        parallel.states ++ Enum.map(parallel.states, &entry_set(&1, targets, idmap))

        # nil ->
        #   IO.inspect(domain_id, label: "domain id")
        #   []
    end
  end

  defp exit_set(domain_id, active_nodes) do
    Enum.filter(active_nodes, fn node ->
      StateNode.descendant?(node.id, domain_id)
    end)
  end

  defp effective_targets(transition_targets, idmap) do
    transition_targets
    |> Enum.map(fn id -> idmap[id] end)
    |> Enum.flat_map(&StateNode.resolve_to_leaves/1)
  end

  defp transition_domain(transition, targets) do
    target_ids = Enum.map(targets, & &1.id)
    %{source_id: source_id} = transition

    if transition.internal && all_descendants_of?(source_id, target_ids) do
      source_id
    else
      StateNode.common_ancestor_id([source_id | target_ids])
    end
  end

  defp loose_descendant?(id1, id2) do
    id1 == id2 || StateNode.descendant?(id1, id2)
  end

  defp all_descendants_of?(id, ids) do
    Enum.all?(ids, &StateNode.descendant?(&1, id))
  end

  @spec select_automatic_transitions(t, State.t()) :: [Transition.t()]
  def select_automatic_transitions(machine, state) do
    nodes = active_nodes(machine, state)

    case first_enabled_transition(nodes, machine, state, state.event, :automatic_transitions) do
      nil -> []
      transition -> [transition]
    end
  end

  @spec select_transitions(t, State.t(), event) :: [Transition.t()]
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
  @spec transition(t, State.t(), sendable_event) :: State.t()
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
  @spec normalize_event(sendable_event) :: event
  def normalize_event({name, data}), do: {name, data}
  def normalize_event(name), do: {name, nil}

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

  @spec active_nodes(t, State.t()) :: [StateNode.t()]
  defp active_nodes(machine, %State{value: value}), do: active_nodes(machine, value)

  defp active_nodes(machine, value) when is_list(value) do
    value
    |> Enum.flat_map(&ancestors(machine, &1))
    |> Enum.uniq()
  end

  @spec ancestors(t, StateNode.id()) :: [StateNode.t()]
  defp ancestors(%Machine{idmap: idmap}, id) do
    id
    |> StateNode.ancestor_ids()
    |> Enum.map(&idmap[&1])
  end

  @spec entry_actions(t, [StateNode.id()]) :: [Action.t()]
  defp entry_actions(machine, ids) do
    machine
    |> entry_order(ids)
    |> Enum.flat_map(& &1.entry)
  end

  defp entry_order(machine, ids), do: document_order(machine, ids)

  defp document_order(machine, ids, sorter \\ :asc) do
    machine
    |> lookup_nodes(ids)
    |> Enum.sort_by(& &1.order, sorter)
  end

  defp lookup_nodes(machine, ids) do
    Enum.map(ids, fn id -> machine.idmap[id] end)
  end
end
