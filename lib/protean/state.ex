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
  alias Protean.Machine
  alias Protean.StateNode

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
          event: Machine.event() | nil,
          context: Machine.context(),
          private: protean_state
        }

  @type value :: [StateNode.id(), ...]

  @opaque protean_state :: %{
            actions: [Action.t()]
          }

  @doc false
  @spec new(value) :: t
  def new(value) when is_list(value),
    do: %State{value: value}

  # Partial Access behaviour (not defining `pop/2`)
  @doc false
  def fetch(state, key), do: Map.fetch(state, key)
  @doc false
  def get_and_update(state, key, fun), do: Map.get_and_update(state, key, fun)

  @doc """
  TODO: Descriptor usage
  """
  @spec matches?(t, StateNode.id()) :: boolean
  @spec matches?(t, String.t()) :: boolean
  @spec matches?(t, atom) :: boolean
  def matches?(state, descriptor)

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
  @spec actions(t) :: [Action.t()]
  def actions(%State{private: %{actions: actions}}), do: actions

  @doc false
  @spec assign_actions(t, [Action.t()]) :: t
  @spec assign_actions(t, nil) :: t
  def assign_actions(state, nil), do: assign_actions(state, [])

  def assign_actions(%State{} = state, actions) when is_list(actions),
    do: put_in(state.private.actions, actions)
end
