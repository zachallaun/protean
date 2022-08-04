defmodule Protean.Machinery do
  @moduledoc """
  Purely-functional statechart core based on the SCXML specification.

  Provides the underlying state-transition logic for a statechart, primarily through
  `transition/3`, a higher-level transition API for using `%Protean.Machine{}` apart from the
  interpreter provided by Protean, and `take_transitions/3`, a lower-level API used by a
  statechart interpreter.

  It is uncommon to use this module independently of the `Protean` behaviour.
  """

  alias Protean.MachineConfig
  alias Protean.Node
  alias Protean.State
  alias Protean.Transition
  alias Protean.Utils

  @doc """
  Return the ids of any ancestors of the given nodes that are in a final state.
  """
  @spec final_ancestors(State.value(), MachineConfig.t(), State.t()) :: [Node.id()]
  def final_ancestors(ids, config, state) do
    for id <- ids do
      parent_id = Node.parent_id(id)
      grandparent_id = Node.parent_id(parent_id)

      if grandparent_id && MachineConfig.fetch!(config, grandparent_id).type == :parallel do
        [parent_id, grandparent_id]
      else
        [parent_id]
      end
    end
    |> Enum.concat()
    |> Enum.uniq()
    |> Enum.filter(&in_final_state?(MachineConfig.fetch!(config, &1), state))
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

  @spec take_transitions(MachineConfig.t(), State.t(), [Transition.t()]) :: State.t()
  def take_transitions(config, state, transitions)

  def take_transitions(_config, state, []), do: state

  def take_transitions(config, state, transitions) do
    [target_ids, to_exit, to_enter] =
      transitions
      |> Enum.map(&transition_result(config, state, &1))
      |> Utils.unzip3()
      |> Tuple.to_list()
      |> Enum.map(fn items -> items |> Enum.concat() |> Enum.uniq() end)

    to_exit = Node.exit_order(to_exit)
    to_enter = Node.entry_order(to_enter)

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
    |> State.assign_active(value)
    |> State.put_actions(actions)
  end

  @spec transition_result(MachineConfig.t(), State.t(), Transition.t()) ::
          {target_ids :: [Node.id()], to_exit :: [Node.t()], to_enter :: [Node.t()]}
  defp transition_result(config, state, transition) do
    current_active = MachineConfig.active(config, state.value)
    target_ids = effective_target_ids(transition.target_ids, config)
    domain = transition_domain(transition, target_ids)
    to_exit = exit_set(domain, current_active)
    to_enter = entry_set(domain, target_ids, config)

    value = (MapSet.to_list(state.value) -- ids(to_exit)) ++ target_ids

    new_active = MachineConfig.active(config, value)

    # if internal, we don't exit states we normally would have exited if they're a part of the
    # new active set
    to_exit =
      if transition.internal do
        MapSet.difference(to_exit, new_active)
      else
        to_exit
      end

    # if internal, we don't enter states we normally would if they were already active
    to_enter =
      if transition.internal do
        MapSet.difference(to_enter, current_active)
      else
        to_enter
      end

    {target_ids, to_exit, to_enter}
  end

  defp entry_set(domain_id, target_ids, config)

  defp entry_set([], target_ids, config),
    do: entry_set(["#"], target_ids, config)

  defp entry_set(_, [], _), do: MapSet.new()

  defp entry_set(domain_id, target_ids, config) do
    case MachineConfig.fetch!(config, domain_id) do
      %{type: :atomic} ->
        MapSet.new()

      %{type: :final} ->
        MapSet.new()

      %{type: :compound} = compound ->
        if child = active_child(compound, target_ids) do
          entry_set(child.id, target_ids, config)
          |> MapSet.put(child)
        else
          MapSet.new()
        end

      %{type: :parallel} = parallel ->
        children = MapSet.new(parallel.states)

        Enum.reduce(children, children, fn child, acc ->
          child.id
          |> entry_set(target_ids, config)
          |> MapSet.union(acc)
        end)
    end
  end

  defp exit_set(domain, active_nodes) do
    active_nodes
    |> Enum.filter(&Node.descendant?(&1.id, domain))
    |> MapSet.new()
  end

  defp effective_target_ids(target_ids, config) do
    target_ids
    |> Enum.map(&MachineConfig.fetch!(config, &1))
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

  @spec select_automatic_transitions(MachineConfig.t(), State.t()) :: [Transition.t()]
  def select_automatic_transitions(config, state) do
    nodes = MachineConfig.active(config, state.value)

    case first_enabled_transition(nodes, config, state, state.event, :automatic_transitions) do
      nil -> []
      transition -> [transition]
    end
  end

  @spec select_transitions(MachineConfig.t(), State.t(), Protean.event()) :: [Transition.t()]
  def select_transitions(config, state, event) do
    # TODO: Handle conflicting transitions
    # TODO: order nodes correctly (specificity + document order)
    nodes = MachineConfig.active(config, state.value)

    case first_enabled_transition(nodes, config, state, event) do
      nil -> []
      transition -> [transition]
    end
  end

  @doc """
  Given a machine, a machine state, and an event, transition to the next state
  if the machine defines a transition for the given state and event.
  """
  @spec transition(MachineConfig.t(), State.t(), Protean.event()) :: State.t()
  def transition(config, state, event) do
    with transitions <- select_transitions(config, state, event) do
      take_transitions(config, state, transitions)
    end
  end

  defp first_enabled_transition(nodes, config, state, event, attribute \\ :transitions) do
    nodes
    |> Enum.flat_map(&Map.get(&1, attribute))
    |> find_enabled_transition(config, state, event)
  end

  defp find_enabled_transition(transitions, config, state, event) do
    Enum.find(transitions, fn transition ->
      Transition.enabled?(transition, event, state, config.callback_module)
    end)
  end

  defp ids(nodes), do: Enum.map(nodes, & &1.id)
end
