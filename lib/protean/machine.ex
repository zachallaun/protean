defmodule Protean.Machine do
  @moduledoc """
  `Protean.Machine` is the purely-functional core of Protean, responsible
  for transitioning a machine from one state to the next as a result of an
  event.
  """

  alias __MODULE__
  alias Protean.MachineConfig
  alias Protean.Node
  alias Protean.State
  alias Protean.Transition
  alias Protean.Utils

  defstruct [
    :root,
    :handler,
    idmap: %{},
    initial_context: %{}
  ]

  @typedoc "A full Protean machine configuration."
  @type t :: %Machine{
          root: Node.t(),
          handler: module(),
          idmap: %{Node.id() => Node.t()},
          initial_context: State.context()
        }

  def new(config, opts \\ []) do
    {root, context} = MachineConfig.parse!(config)
    idmap = Utils.Tree.tree_reduce(root, &idmap_reducer/2, %{})

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
      |> Node.resolve_to_leaves()
      |> Enum.map(& &1.id)

    entry_ids =
      active_ids
      |> Enum.flat_map(&Node.ancestor_ids/1)
      |> Enum.uniq()

    State.new(active_ids)
    |> State.assign(context)
    |> State.assign_actions(entry_actions(machine, entry_ids))
  end

  @doc false
  @spec final_ancestors(State.value(), t, State.t()) :: [Node.id()]
  def final_ancestors(ids, machine, state) do
    %Machine{idmap: idmap} = machine

    for id <- ids do
      parent_id = Node.parent_id(id)
      grandparent_id = Node.parent_id(parent_id)

      if grandparent_id && idmap[grandparent_id].type == :parallel do
        [parent_id, grandparent_id]
      else
        [parent_id]
      end
    end
    |> Enum.concat()
    |> Enum.uniq()
    |> Enum.filter(&in_final_state?(idmap[&1], state))
  end

  @spec in_final_state?(Node.t(), State.t()) :: boolean()
  defp in_final_state?(%Node{type: :atomic}, _), do: false

  defp in_final_state?(%Node{type: :final} = node, state) do
    node.id in state.value
  end

  defp in_final_state?(%Node{type: :compound} = node, state) do
    node
    |> active_child(state.value)
    |> in_final_state?(state)
  end

  defp in_final_state?(%Node{type: :parallel} = node, state) do
    Enum.all?(node.states, &in_final_state?(&1, state))
  end

  defp active_child(%Node{type: :compound} = node, active_ids) do
    Enum.find(node.states, fn child ->
      Enum.any?(active_ids, &loose_descendant?(&1, child.id))
    end)
  end

  @spec take_transitions(t, State.t(), [Transition.t()]) :: State.t()
  def take_transitions(machine, state, transitions)

  def take_transitions(_machine, state, []), do: state

  def take_transitions(machine, state, transitions) do
    [target_ids, to_exit, to_enter] =
      transitions
      |> Enum.map(&transition_result(machine, state, &1))
      |> Utils.unzip3()
      |> Tuple.to_list()
      |> Enum.map(fn items -> items |> Enum.concat() |> Enum.uniq() end)

    to_exit = exit_order(to_exit)
    to_enter = entry_order(to_enter)

    value =
      MapSet.to_list(state.value)
      |> then(&(&1 -- Enum.map(to_exit, fn node -> node.id end)))
      |> Enum.concat(target_ids)
      |> Enum.uniq()

    actions =
      Enum.flat_map(to_exit, & &1.exit) ++
        Enum.flat_map(transitions, & &1.actions) ++
        Enum.flat_map(to_enter, & &1.entry)

    state
    |> State.assign_value(value)
    |> State.put_actions(actions)
  end

  @spec transition_result(t, State.t(), Transition.t()) ::
          {target_ids :: [Node.id()], to_exit :: [Node.t()], to_enter :: [Node.t()]}
  defp transition_result(machine, state, transition) do
    %{idmap: idmap} = machine
    active = active_nodes(machine, state.value)
    target_ids = effective_target_ids(transition.target_ids, idmap)
    domain = transition_domain(transition, target_ids)
    to_exit = exit_set(domain, active)
    to_enter = entry_set(domain, target_ids, idmap)

    value = (MapSet.to_list(state.value) -- ids(to_exit)) ++ target_ids

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

    {target_ids, to_exit, to_enter}
  end

  defp entry_set(domain_id, target_ids, idmap)

  defp entry_set([], target_ids, idmap),
    do: entry_set(["#"], target_ids, idmap)

  defp entry_set(_, [], _), do: []

  defp entry_set(domain_id, target_ids, idmap) do
    case idmap[domain_id] do
      %{type: :atomic} ->
        []

      %{type: :final} ->
        []

      %{type: :compound} = compound ->
        if child = active_child(compound, target_ids) do
          [child | entry_set(child.id, target_ids, idmap)]
        else
          []
        end

      %{type: :parallel} = parallel ->
        parallel.states ++ Enum.flat_map(parallel.states, &entry_set(&1.id, target_ids, idmap))
    end
  end

  defp exit_set(domain, active_nodes) do
    Enum.filter(active_nodes, &Node.descendant?(&1.id, domain))
  end

  defp effective_target_ids(target_ids, idmap) do
    target_ids
    |> Enum.map(fn id -> idmap[id] end)
    |> Enum.flat_map(&Node.resolve_to_leaves/1)
    |> ids()
  end

  @spec transition_domain(Transition.t(), [Node.id()]) :: Node.id()
  defp transition_domain(transition, target_ids) do
    %{source_id: source_id} = transition

    if transition.internal && all_descendants_of?(source_id, target_ids) do
      source_id
    else
      Node.common_ancestor_id([source_id | target_ids])
    end
  end

  defp loose_descendant?(id1, id2) do
    id1 == id2 || Node.descendant?(id1, id2)
  end

  defp all_descendants_of?(id, ids) do
    Enum.all?(ids, &Node.descendant?(&1, id))
  end

  @spec select_automatic_transitions(t, State.t()) :: [Transition.t()]
  def select_automatic_transitions(machine, state) do
    nodes = active_nodes(machine, state.value)

    case first_enabled_transition(nodes, machine, state, state.event, :automatic_transitions) do
      nil -> []
      transition -> [transition]
    end
  end

  @spec select_transitions(t, State.t(), Protean.event()) :: [Transition.t()]
  def select_transitions(machine, state, event) do
    # TODO: Handle conflicting transitions
    # TODO: order nodes correctly (specificity + document order)
    nodes = active_nodes(machine, state.value)

    case first_enabled_transition(nodes, machine, state, event) do
      nil -> []
      transition -> [transition]
    end
  end

  @doc """
  Given a machine, a machine state, and an event, transition to the next state
  if the machine defines a transition for the given state and event.
  """
  @spec transition(t, State.t(), Protean.event()) :: State.t()
  def transition(machine, state, event) do
    with transitions <- select_transitions(machine, state, event) do
      take_transitions(machine, state, transitions)
    end
  end

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

  @spec active_nodes(t, State.value()) :: [Node.t()]
  defp active_nodes(machine, value) do
    value
    |> Enum.flat_map(&ancestors(machine, &1))
    |> Enum.uniq()
  end

  @spec ancestors(t, Node.id()) :: [Node.t()]
  defp ancestors(%Machine{idmap: idmap}, id) do
    id
    |> Node.ancestor_ids()
    |> Enum.map(&idmap[&1])
  end

  @spec entry_actions(t, [Node.id()]) :: [Action.t()]
  defp entry_actions(machine, ids) do
    machine
    |> lookup_nodes(ids)
    |> entry_order()
    |> Enum.flat_map(& &1.entry)
  end

  defp entry_order(nodes) do
    Enum.sort_by(nodes, & &1.order, :asc)
  end

  defp exit_order(nodes) do
    Enum.sort_by(nodes, & &1.order, :desc)
  end

  defp lookup_nodes(machine, ids) do
    Enum.map(ids, fn id -> machine.idmap[id] end)
  end

  defp ids(nodes), do: Enum.map(nodes, & &1.id)
end
