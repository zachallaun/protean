defmodule Protean.State do
  @moduledoc """
  Snapshot of active states, assigns, and the latest event seen by the machine.

  This module is rarely interacted with directly. Instead, use the API exposed by the top-level
  `Protean`, such as `Protean.matches?/2`, or actions exposed by `Protean.Action`.
  """

  alias __MODULE__
  alias Protean.Action
  alias Protean.Node

  @derive {Inspect, only: [:value, :event, :assigns]}
  defstruct [
    :value,
    :event,
    final: MapSet.new(),
    assigns: %{},
    private: %{
      actions: [],
      replies: []
    }
  ]

  @type t :: %State{
          value: value,
          final: value,
          event: Protean.event() | nil,
          assigns: assigns,
          private: private_state
        }

  @type value :: MapSet.t(Node.id())

  @type assigns :: %{any => any}

  @opaque private_state :: %{
            actions: [Action.unresolved()],
            replies: [term()]
          }

  @doc false
  @spec new(Enumerable.t()) :: t
  def new(value), do: %State{value: MapSet.new(value)}

  # Partial Access behaviour (not defining `pop/2`)
  @doc false
  def fetch(state, key), do: Map.fetch(state, key)
  @doc false
  def get_and_update(state, key, fun), do: Map.get_and_update(state, key, fun)

  @spec matches?(t, Node.id()) :: boolean()
  @spec matches?(t, String.t()) :: boolean()
  @spec matches?(t, atom()) :: boolean()
  def matches?(state, descriptor)

  def matches?(%State{value: value}, query) when is_list(query) do
    Enum.any?(value, fn id -> id == query || Node.descendant?(id, query) end)
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

  @doc false
  @spec assign_active(t, [Node.id(), ...]) :: State.t()
  def assign_active(state, ids) do
    %{state | value: MapSet.new(ids)}
  end

  @doc false
  @spec assign_final(t, MapSet.t(Node.id())) :: State.t()
  def assign_final(state, ids) do
    %{state | final: ids}
  end

  # Assign data to a state's assigns.
  #
  # Usage:
  #
  #   * `assign(state, key, value)` - Assigns value to key in state's assigns.
  #   * `assign(state, %{})` - Merges the update map into a state's assigns.
  #   * `assign(state, enumerable)` - Collects the key/values of `enumerable` into a map, then
  #     merges that map into the state's assigns.
  @doc false
  @spec assign(t, any, any) :: t
  @spec assign(t, %{any => any}) :: t
  @spec assign(t, Enumerable.t()) :: t
  def assign(%State{assigns: assigns} = state, key, value),
    do: %{state | assigns: Map.put(assigns, key, value)}

  def assign(%State{assigns: assigns} = state, updates) when is_map(updates),
    do: %{state | assigns: Map.merge(assigns, updates)}

  def assign(state, enum),
    do: assign(state, Enum.into(enum, %{}))

  @doc false
  def actions(state), do: state.private.actions

  @doc false
  def assign_actions(state, actions \\ []),
    do: put_in(state.private.actions, actions)

  @doc false
  def update_actions(state, fun),
    do: update_in(state.private.actions, fun)

  @doc false
  def put_actions(state, actions),
    do: update_actions(state, &(&1 ++ actions))

  @doc false
  def pop_actions(state),
    do: {actions(state), assign_actions(state)}

  @doc false
  def put_reply(state, reply),
    do: update_in(state.private.replies, &[reply | &1])

  @doc false
  def get_replies(state),
    do: state.private.replies |> Enum.reverse()

  @doc false
  def pop_replies(state),
    do: {get_replies(state), put_in(state.private.replies, [])}
end
