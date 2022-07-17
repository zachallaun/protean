defmodule Protean.MachineConfig do
  @moduledoc """
  This module provides utilities to convert from the "shorthand" machine config
  format used when defining a machine to the structured format used internally.
  """

  alias Protean.{StateNode, Transition}

  @doc """
  Parses semi-structured machine config into a `StateNode`.
  """
  def parse!(config) do
    context = Keyword.get(config, :context, %{})

    {root, _order} =
      config
      |> node_type()
      |> parse_node(config)
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
      config[:states] -> :parallel
      true -> :atomic
    end
  end

  defp parse_node(type, config, id \\ ["#"])

  defp parse_node(:atomic, config, id) do
    forbid!(config, [:states, :initial])

    %StateNode{
      type: :atomic,
      id: id,
      transitions: parse_transitions(id, config[:on]),
      entry: parse_actions(config[:entry]),
      exit: parse_actions(config[:exit])
    }
  end

  defp parse_node(:final, config, id) do
    forbid!(config, [:states, :initial, :on])

    %StateNode{
      type: :final,
      id: id,
      entry: parse_actions(config[:entry]),
      exit: parse_actions(config[:exit])
    }
  end

  defp parse_node(:compound, config, id) do
    require!(config, [:states, :initial])

    %StateNode{
      type: :compound,
      id: id,
      states: parse_children(id, config[:states]),
      initial: parse_target(config[:initial]) ++ id,
      transitions: parse_transitions(id, config[:on]),
      entry: parse_actions(config[:entry]),
      exit: parse_actions(config[:exit])
    }
  end

  defp parse_node(:parallel, config, id) do
    require!(config, [:states])
    forbid!(config, [:initial])

    %StateNode{
      type: :parallel,
      id: id,
      states: parse_children(id, config[:states]),
      transitions: parse_transitions(id, config[:on]),
      entry: parse_actions(config[:entry]),
      exit: parse_actions(config[:exit])
    }
  end

  defp parse_children(id, children) do
    for {name, child_config} <- children,
        name = to_string(name) do
      child_id = [name | id]
      child_config |> node_type() |> parse_node(child_config, child_id)
    end
  end

  defp parse_actions(nil), do: []
  defp parse_actions(actions), do: actions

  defp parse_transitions(_id, nil), do: []

  defp parse_transitions(id, transitions),
    do: Enum.map(transitions, &parse_transition(id, &1))

  defp parse_transition(id, {descriptor, target}) when is_atom(target),
    do: parse_transition(id, {descriptor, [target: target]})

  defp parse_transition(id, {descriptor, transition}) do
    %Transition{
      event_descriptor: parse_event_descriptor(descriptor),
      actions: parse_actions(transition[:actions]),
      targets: resolve_targets(transition[:target], id),
      guard: parse_guard(transition[:when])
    }
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

  defp resolve_targets(nil, _id), do: []

  defp resolve_targets(targets, _id) when is_list(targets),
    do: raise("Multiple targets not yet implemented")

  defp resolve_targets(target, [_self | ancestors]),
    do: [resolve_target(target, ancestors)]

  defp resolve_target(target, ancestors) when is_atom(target),
    do: resolve_target(to_string(target), ancestors)

  defp resolve_target("#" <> target, _ancestors) when is_binary(target) do
    target
    |> parse_target()
    |> List.insert_at(-1, "#")
  end

  defp resolve_target(target, ancestors) when is_binary(target) do
    relative = parse_target(target)
    relative ++ ancestors
  end

  defp parse_target(target) when is_atom(target),
    do: parse_target(to_string(target))

  defp parse_target(target) when is_binary(target) do
    target
    |> String.split(".")
    |> Enum.reverse()
  end

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
end
