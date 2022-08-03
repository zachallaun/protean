defmodule Protean.State do
  @moduledoc """
  The Protean machine state.

  A `%Protean.State{}` struct encompasses the current state of a machine, including its state
  configuration (value), the event that triggered a transition to this state, the current context
  (extended state), and other private state used internally by Protean.

  Additionally, the action callback `c:pure/3` receives and returns state to update the context
  and schedule side-effects. See `Protean.Action`.
  """

  alias __MODULE__
  alias Protean.Action
  alias Protean.Node

  @derive {Inspect, only: [:value, :event, :context]}
  defstruct [
    :value,
    :event,
    context: %{},
    private: %{
      actions: []
    }
  ]

  @type t :: %State{
          value: value,
          event: Protean.event() | nil,
          context: context,
          private: private_state
        }

  @type value :: MapSet.t(Node.id())

  @type context :: %{any => any}

  @opaque private_state :: %{
            actions: [Action.unresolved()]
          }

  @doc false
  @spec new(Enumerable.t()) :: t
  def new(value), do: %State{value: MapSet.new(value)}

  # Partial Access behaviour (not defining `pop/2`)
  @doc false
  def fetch(state, key), do: Map.fetch(state, key)
  @doc false
  def get_and_update(state, key, fun), do: Map.get_and_update(state, key, fun)

  @doc """
  TODO: Descriptor usage
  """
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

  @doc """
  Assign data to a state's context.

  Usage:

    * `assign(state, key, value)` - Assigns value to key in state's context.
    * `assign(state, %{})` - Merges the update map into a state's context.
    * `assign(state, enumerable)` - Collects the key/values of `enumerable` into a map, then
      merges that map into the state's context.
  """
  @spec assign(t, any, any) :: t
  @spec assign(t, %{any => any}) :: t
  @spec assign(t, Enumerable.t()) :: t
  def assign(%State{context: context} = state, key, value),
    do: %{state | context: Map.put(context, key, value)}

  def assign(%State{context: context} = state, updates) when is_map(updates),
    do: %{state | context: Map.merge(context, updates)}

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
  def assign_value(state, value),
    do: %{state | value: MapSet.new(value)}
end
