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
  alias Protean.Context
  alias Protean.Transition

  @doc "Transition in response to an event."
  @spec transition(MachineConfig.t(), Context.t(), Protean.event()) :: Context.t()
  def transition(config, context, event) do
    with transitions <- select_transitions(config, context, event) do
      take_transitions(config, context, transitions)
    end
  end

  @doc "Select any machine transitions that apply to the given event in the current context."
  @spec select_transitions(MachineConfig.t(), Context.t(), Protean.event()) :: [Transition.t()]
  def select_transitions(config, context, event, attribute \\ :transitions) do
    # TODO: Handle conflicting transitions
    # TODO: order nodes correctly (specificity + document order)
    config
    |> MachineConfig.active(context.value)
    |> first_enabled_transition(config, context, event, attribute)
    |> List.wrap()
  end

  @spec take_transitions(MachineConfig.t(), Context.t(), [Transition.t()]) :: Context.t()
  def take_transitions(config, context, transitions)

  def take_transitions(_config, context, []), do: context

  def take_transitions(config, context, transitions) do
    {exit_sets, entry_sets} =
      transitions
      |> Enum.map(&transition_result(config, context, &1))
      |> Enum.unzip()

    to_exit = exit_sets |> Enum.reduce(&MapSet.union/2) |> Node.exit_order()
    to_enter = entry_sets |> Enum.reduce(&MapSet.union/2) |> Node.entry_order()

    value =
      context.value
      |> MapSet.difference(leaf_ids(to_exit))
      |> MapSet.union(leaf_ids(to_enter))

    actions =
      Enum.concat([
        Enum.flat_map(to_exit, &Node.exit_actions/1),
        Enum.flat_map(transitions, &Transition.actions/1),
        Enum.flat_map(to_enter, &Node.entry_actions/1)
      ])

    final_states = final_ancestors(config, value)

    context
    |> Context.assign_active(value)
    |> Context.assign_final(final_states)
    |> Context.put_actions(actions)
  end

  defp transition_result(config, context, transition) do
    domain = Transition.domain(transition)

    active_in_scope =
      config
      |> MachineConfig.active(context.value)
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

  defp first_enabled_transition(nodes, config, context, event, attribute) do
    nodes
    |> Enum.flat_map(&Map.get(&1, attribute))
    |> find_enabled_transition(config, context, event)
  end

  defp find_enabled_transition(transitions, config, context, event) do
    Enum.find(transitions, fn transition ->
      Transition.enabled?(transition, event, context, config.callback_module)
    end)
  end

  # Return the ancestors of the given active leaves that are in a final state
  @spec final_ancestors(MachineConfig.t(), Context.value()) :: MapSet.t(Node.id())
  defp final_ancestors(config, ids) do
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
    |> Enum.filter(&in_final_state?(MachineConfig.fetch!(config, &1), ids))
    |> MapSet.new()
  end

  @spec in_final_state?(Node.t(), Context.value()) :: boolean()
  defp in_final_state?(%Node{type: :atomic}, _), do: false

  defp in_final_state?(%Node{type: :final} = node, value) do
    node.id in value
  end

  defp in_final_state?(%Node{type: :compound} = node, value) do
    node
    |> active_child(value)
    |> in_final_state?(value)
  end

  defp in_final_state?(%Node{type: :parallel} = node, value) do
    Enum.all?(node.states, &in_final_state?(&1, value))
  end

  defp active_child(%Node{type: :compound} = node, active_ids) do
    Enum.find(node.states, fn child ->
      Enum.any?(active_ids, &loose_descendant?(&1, child.id))
    end)
  end
end
