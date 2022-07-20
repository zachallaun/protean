defmodule Protean.MachineConfig do
  @moduledoc """
  This module provides utilities to convert from the "shorthand" machine config
  format used when defining a machine to the structured format used internally.
  """

  alias Protean.Action
  alias Protean.StateNode
  alias Protean.Transition

  @doc """
  Parses machine config into a `StateNode`.
  """
  def parse!(config) do
    context = Keyword.get(config, :context, %{})

    {root, _order} =
      config
      |> parse_node(["#"])
      |> set_order()

    {root, context}
  end

  defp set_order(node, order \\ 0)

  defp set_order(%{states: nil} = node, order),
    do: {%StateNode{node | order: order}, order + 1}

  defp set_order(%{states: []} = node, order),
    do: {%StateNode{node | order: order}, order + 1}

  defp set_order(%{states: children} = node, order) do
    node = %StateNode{node | order: order}

    {children, order} =
      Enum.reduce(children, {[], order + 1}, fn child, {already_set, order} ->
        {child, order} = set_order(child, order)
        {[child | already_set], order}
      end)

    {%StateNode{node | states: Enum.reverse(children)}, order}
  end

  defp node_type(config) do
    cond do
      type = config[:type] -> type
      config[:initial] -> :compound
      true -> :atomic
    end
  end

  defp parse_node(config, id) do
    config
    |> node_type()
    |> parse_node(config, id)
  end

  defp parse_node(type, config, id)

  defp parse_node(:atomic, config, id) do
    forbid!(config, [:states, :initial])

    parse_node_common(:atomic, config, id)
  end

  defp parse_node(:final, config, id) do
    forbid!(config, [:states, :initial, :on, :always, :after])

    parse_node_common(:final, config, id)
  end

  defp parse_node(:compound, config, id) do
    require!(config, [:states, :initial])

    parse_node_common(:compound, config, id)
  end

  defp parse_node(:parallel, config, id) do
    require!(config, [:states])
    forbid!(config, [:initial])

    parse_node_common(:parallel, config, id)
  end

  defp parse_node_common(type, config, id) do
    {delay_entry, delay_exit, delay_transitions} = parse_delayed_transitions(config[:after], id)

    %StateNode{
      type: type,
      id: id,
      states: parse_children(config[:states], id),
      initial: parse_initial(config[:initial], id),
      automatic_transitions: parse_automatic_transitions(config[:always], id),
      transitions: delay_transitions ++ parse_transitions(config[:on], id),
      entry: delay_entry ++ parse_actions(config[:entry]),
      exit: delay_exit ++ parse_actions(config[:exit])
    }
  end

  defp parse_delayed_transitions(nil, _id), do: {[], [], []}

  defp parse_delayed_transitions(transitions, id) do
    if Keyword.keyword?(transitions) do
      parse_delayed_transitions([transitions], id)
    else
      transitions
      |> Enum.map(&parse_delayed_transition(&1, id))
      |> unzip3()
    end
  end

  defp parse_delayed_transition(config, id) do
    {delay, config} = Keyword.pop!(config, :delay)
    event_name = delayed_transition_event_name(id, delay)

    entry_action = parse_action(Action.send_event(event_name, delay: delay))
    exit_action = parse_action(Action.cancel_event(event_name))
    transition = parse_transition(config ++ [on: event_name], id)

    {entry_action, exit_action, transition}
  end

  defp delayed_transition_event_name(id, delay) do
    ["#" | rest] = Enum.reverse(id)
    "$protean.after.#{delay}-#" <> Enum.join(rest, ".")
  end

  defp parse_children(nil, _id), do: nil

  defp parse_children(children, id) do
    for {name, child_config} <- children,
        name = to_string(name) do
      child_id = [name | id]
      parse_node(child_config, child_id)
    end
  end

  defp parse_actions(nil), do: []
  defp parse_actions(actions), do: Enum.map(actions, &parse_action/1)

  defp parse_action(action), do: action

  defp parse_automatic_transitions(nil, _id), do: []

  defp parse_automatic_transitions(target, id) when is_atom(target) or is_binary(target),
    do: [parse_transition(target, id)]

  defp parse_automatic_transitions(transitions, id) when is_list(transitions) do
    if Keyword.keyword?(transitions) do
      [parse_transition(transitions, id)]
    else
      parse_transitions(transitions, id)
    end
  end

  defp parse_transitions(nil, _id), do: []

  defp parse_transitions(transitions, id),
    do: Enum.map(transitions, &parse_transition(&1, id))

  defp parse_transition({descriptor, target}, id) when is_atom(target) or is_binary(target),
    do: parse_transition([target: target, on: descriptor], id)

  defp parse_transition({descriptor, transition}, id) when is_list(transition),
    do: parse_transition([on: descriptor] ++ transition, id)

  defp parse_transition(target, id) when is_atom(target) or is_binary(target),
    do: parse_transition([target: target], id)

  defp parse_transition(transition, id) when is_list(transition) do
    targets = resolve_targets(transition[:target], id)

    %Transition{
      source_id: id,
      event_descriptor: parse_event_descriptor(transition[:on]),
      actions: parse_actions(transition[:actions]),
      targets: targets,
      guard: parse_guard(transition[:when])
    }
    |> add_internal(targets, transition[:internal])
  end

  defp add_internal(transition, targets, internal) do
    case {transition.source_id, targets, internal} do
      {id, [id], nil} -> %{transition | internal: true}
      {id, [id], false} -> %{transition | internal: false}
      {id, [id], _} -> %{transition | internal: true}
      {_, _, true} -> %{transition | internal: true}
      {_, _, _} -> %{transition | internal: false}
    end
  end

  defp parse_guard(nil), do: nil

  defp parse_guard(guard) when is_binary(guard) or is_tuple(guard),
    do: guard

  defp parse_guard([:not | guards]),
    do: {:not, parse_guard(guards)}

  defp parse_guard([:and | guards]),
    do: {:and, Enum.map(guards, &parse_guard/1)}

  defp parse_guard([:or | guards]),
    do: {:or, Enum.map(guards, &parse_guard/1)}

  defp parse_guard([guard]),
    do: parse_guard(guard)

  defp parse_guard([_ | _] = guards),
    do: parse_guard([:and | guards])

  defp parse_guard([]),
    do: []

  defp resolve_targets(nil, id), do: [id]

  defp resolve_targets(targets, _id) when is_list(targets),
    do: raise("Multiple targets not yet implemented")

  defp resolve_targets(target, id),
    do: [resolve_target(target, id)]

  defp resolve_target(target, id) when is_atom(target),
    do: resolve_target(to_string(target), id)

  defp resolve_target("#" <> target, _id) do
    target
    |> parse_target()
    |> List.insert_at(-1, "#")
  end

  defp resolve_target("." <> target, id) do
    relative = parse_target(target)
    relative ++ id
  end

  defp resolve_target(target, [_self | ancestors]) when is_binary(target) do
    relative = parse_target(target)
    relative ++ ancestors
  end

  defp parse_initial(nil, _id), do: nil

  defp parse_initial(target, id),
    do: parse_target(target) ++ id

  defp parse_target(target) when is_atom(target),
    do: parse_target(to_string(target))

  defp parse_target(target) when is_binary(target) do
    target
    |> String.split(".")
    |> Enum.reverse()
  end

  defp parse_event_descriptor(nil), do: nil

  defp parse_event_descriptor(descriptor) when is_atom(descriptor),
    do: parse_event_descriptor(to_string(descriptor))

  defp parse_event_descriptor(descriptor) when is_binary(descriptor) do
    descriptor
    |> String.split(" ")
    |> Enum.map(&String.split(&1, "."))
    |> Enum.map(&expand_descriptor_component/1)
  end

  defp expand_descriptor_component([]), do: []
  defp expand_descriptor_component(["" | rest]), do: expand_descriptor_component(rest)
  defp expand_descriptor_component(["*" | rest]), do: expand_descriptor_component(rest)
  defp expand_descriptor_component([part | rest]), do: [part | expand_descriptor_component(rest)]

  defp require!(config, keys),
    do: check!(config, keys, &is_nil(config[&1]), "must have keys")

  defp forbid!(config, keys),
    do: check!(config, keys, &config[&1], "cannot have keys")

  defp check!(config, keys, filter_fun, message) do
    filtered = Enum.filter(keys, filter_fun)

    unless Enum.empty?(filtered) do
      raise "#{message} #{Enum.join(filtered, ", ")}: #{inspect(config)}"
    end
  end

  defp unzip3(list),
    do: unzip3(Enum.reverse(list), [], [], [])

  defp unzip3([{el1, el2, el3} | reversed_list], l1, l2, l3),
    do: unzip3(reversed_list, [el1 | l1], [el2 | l2], [el3 | l3])

  defp unzip3([], l1, l2, l3),
    do: {l1, l2, l3}
end
