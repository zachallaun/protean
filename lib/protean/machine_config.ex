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
    parse_node(node_type(config), config)
  end

  defp node_type(config) do
    cond do
      type = config[:type] -> type
      config[:states] -> :compound
      true -> :atomic
    end
  end

  defp parse_node(type, config, id \\ [:"#"])

  defp parse_node(:atomic, config, id) do
    forbid!(config, [:states, :initial])

    %StateNode{
      type: :atomic,
      id: id,
      transitions: parse_transitions(id, config[:on])
    }
  end

  defp parse_node(:final, config, id) do
    forbid!(config, [:states, :initial, :on])

    %StateNode{
      type: :final,
      id: id
    }
  end

  defp parse_node(:compound, config, id) do
    require!(config, [:states, :initial])

    children =
      for {name, child_config} <- config[:states] do
        child_id = [name | id]
        child_config |> node_type() |> parse_node(child_config, child_id)
      end

    %StateNode{
      type: :compound,
      id: id,
      states: children,
      initial: parse_target(config[:initial]),
      transitions: parse_transitions(id, config[:on])
    }
  end

  defp parse_transitions(_id, nil), do: nil

  defp parse_transitions(id, transitions),
    do: Enum.map(transitions, &parse_transition(id, &1))

  defp parse_transition(id, {descriptor, target}) when is_atom(target),
    do: parse_transition(id, {descriptor, [target: target]})

  defp parse_transition([_self | ancestors], {descriptor, transition}) do
    require!(transition, [:target])

    %Transition{
      event_descriptor: parse_event_descriptor(descriptor),
      targets: [resolve_target(transition[:target], ancestors)]
    }
  end

  defp resolve_target(target, ancestors) when is_atom(target),
    do: resolve_target(to_string(target), ancestors)

  defp resolve_target("#" <> target, _ancestors) when is_binary(target) do
    target
    |> parse_target()
    |> List.insert_at(-1, :"#")
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
    |> Enum.map(&String.to_atom/1)
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
