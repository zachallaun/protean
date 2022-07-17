defmodule Protean.State do
  @moduledoc false

  alias __MODULE__
  alias Protean.{Machine, StateNode, Action}

  defstruct [
    :value,
    :event,
    context: %{},
    actions: []
  ]

  @type t :: %State{
          value: value,
          event: Machine.event() | nil,
          context: Machine.context(),
          actions: [Action.t()]
        }

  @type value :: [StateNode.id(), ...]

  @doc "See `Protean.matches?/2`."
  @spec matches?(State.t(), StateNode.id() | String.t() | atom) :: boolean
  def matches?(state, query)

  def matches?(%State{value: value}, query) when is_list(query) do
    Enum.any?(value, fn id -> id == query || StateNode.descendant?(id, query) end)
  end

  def matches?(state, query) when is_binary(query) do
    matches?(state, parse_match_query(query))
  end

  def matches?(state, query) when is_atom(query) do
    matches?(state, to_string(query))
  end

  defp parse_match_query(""), do: ["#"]
  defp parse_match_query("#"), do: ["#"]
  defp parse_match_query("#." <> query), do: parse_match_query(query)
  defp parse_match_query("#" <> query), do: parse_match_query(query)

  defp parse_match_query(query) do
    query
    |> String.split(".")
    |> List.insert_at(0, "#")
    |> Enum.reverse()
  end
end
