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
    [to_exit, to_enter] =
      transitions
      |> Enum.map(&transition_result(config, state, &1))
      |> Enum.unzip()
      |> Tuple.to_list()
      |> Enum.map(fn items -> items |> Enum.concat() |> Enum.uniq() end)

    to_exit = Node.exit_order(to_exit)
    to_enter = Node.entry_order(to_enter)

    value =
      state.value
      |> MapSet.difference(leaf_ids(to_exit))
      |> MapSet.union(leaf_ids(to_enter))

    actions =
      Enum.flat_map(to_exit, & &1.exit) ++
        Enum.flat_map(transitions, & &1.actions) ++
        Enum.flat_map(to_enter, & &1.entry)

    state
    |> State.assign_active(value)
    |> State.put_actions(actions)
  end

  defp transition_result(config, state, transition) do
    domain = Transition.domain(transition)

    active_in_scope =
      config
      |> MachineConfig.active(state.value)
      |> Enum.filter(&Node.descendant?(&1.id, domain))
      |> MapSet.new()

    targets = effective_targets(config, transition)

    to_exit =
      Enum.filter(active_in_scope, fn node ->
        !Enum.all?(targets, fn target ->
          (transition.internal && node == target) || Node.descendant?(node.id, target.id)
        end)
      end)
      |> MapSet.new()

    remaining_active = MapSet.difference(active_in_scope, to_exit)

    to_enter = MapSet.difference(targets, remaining_active)

    {to_exit, to_enter}
  end

  @spec effective_targets(MachineConfig.t(), Transition.t()) :: MapSet.t(Node.t())
  defp effective_targets(config, %Transition{internal: true} = t) do
    domain = Transition.domain(t)

    t.target_ids
    |> Enum.flat_map(&MachineConfig.lineage(config, &1))
    |> Enum.filter(&Node.descendant?(&1.id, domain))
    |> MapSet.new()
  end

  defp effective_targets(config, %Transition{internal: false} = t) do
    domain = Transition.domain(t)

    config
    |> MachineConfig.active(t.target_ids)
    |> Enum.filter(&Node.descendant?(&1.id, domain))
    |> MapSet.new()
  end

  defp leaf_ids(nodes) do
    nodes
    |> Enum.filter(&Node.leaf?/1)
    |> Enum.map(& &1.id)
    |> MapSet.new()
  end

  defp loose_descendant?(id1, id2) do
    id1 == id2 || Node.descendant?(id1, id2)
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
end
