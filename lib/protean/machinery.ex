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

  @spec transition_result(MachineConfig.t(), State.t(), Transition.t()) ::
          {target_ids :: [Node.id()], to_exit :: [Node.t()], to_enter :: [Node.t()]}
  defp transition_result(config, state, transition) do
    current_active = MachineConfig.active(config, state.value)
    domain = Transition.domain(transition)

    to_exit =
      cond do
        is_nil(domain) -> MapSet.new()
        transition.internal -> internal_exit_set(domain, current_active, transition.target_ids)
        true -> external_exit_set(domain, current_active)
      end

    remaining_active = MapSet.difference(current_active, to_exit)

    to_enter =
      MachineConfig.active(config, transition.target_ids)
      |> Enum.filter(fn node ->
        if transition.internal do
          Node.descendant?(node.id, domain) &&
            !Enum.any?(transition.target_ids, &Node.descendant?(node.id, &1))
        else
          Node.descendant?(node.id, domain)
        end
      end)
      |> MapSet.new()
      |> MapSet.difference(remaining_active)

    case {Enum.empty?(to_exit), Enum.empty?(to_enter)} do
      {true, true} ->
        :ok

      {false, false} ->
        :ok

      _ ->
        values = [
          internal: transition.internal,
          current: state.value,
          source_id: transition.source_id,
          target_ids: transition.target_ids,
          domain: domain,
          to_exit: Enum.map(to_exit, & &1.id),
          to_enter: Enum.map(to_enter, & &1.id),
          remaining: Enum.map(remaining_active, & &1.id)
        ]

        raise "One of exit or entry sets is empty but the other isn't. This means Protean made a boo-boo:\n#{inspect(values)}"
    end

    {to_exit, to_enter}
  end

  # On internal, exit any descendants of the domain that aren't a target or parent of a target
  defp internal_exit_set(domain, active, target_ids) do
    Enum.filter(active, fn node ->
      Node.descendant?(node.id, domain) &&
        !Enum.any?(target_ids, fn tid -> tid == node.id || Node.descendant?(node.id, tid) end)
    end)
    |> MapSet.new()
  end

  # On external transitions, exit any nodes that are descendants of the transition domain.
  defp external_exit_set(domain_id, active) do
    Enum.filter(active, fn node ->
      Node.descendant?(node.id, domain_id)
    end)
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
