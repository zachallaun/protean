defmodule Protean.Context do
  @moduledoc """
  Snapshot of active states, assigns, and the latest event seen by the machine.

  Functions in this module should rarely be used directly. Instead, rely on the API exposed by
  `Protean` and `Protean.Action` to query and modify machine context.
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

  @type t :: %Context{
          value: value,
          final: value,
          event: Protean.event() | nil,
          assigns: assigns,
          private: private_state
        }

  @type value :: MapSet.t(Node.id())

  @type assigns :: %{any => any}

  @opaque private_state :: %{
            actions: [Action.t()],
            replies: [term()]
          }

  @doc false
  @spec new(Enumerable.t()) :: t
  def new(value), do: %Context{value: MapSet.new(value)}

  # Partial Access behaviour (not defining `pop/2`)
  @doc false
  def fetch(context, key), do: Map.fetch(context, key)
  @doc false
  def get_and_update(context, key, fun), do: Map.get_and_update(context, key, fun)

  @spec matches?(t, Node.id() | String.t() | atom()) :: boolean()
  def matches?(context, descriptor)

  def matches?(%Context{value: value}, query) when is_list(query) do
    Enum.any?(value, fn id -> id == query || Node.descendant?(id, query) end)
  end

  def matches?(context, query) when is_binary(query) do
    matches?(context, parse_match_query(query))
  end

  def matches?(context, query) when is_atom(query) do
    matches?(context, to_string(query))
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
  @spec assign_active(t, Enumerable.t()) :: t
  def assign_active(context, ids) do
    %{context | value: MapSet.new(ids)}
  end

  @doc false
  @spec assign_final(t, MapSet.t(Node.id())) :: t
  def assign_final(context, ids) do
    %{context | final: ids}
  end

  # Assign data to a context's assigns.
  #
  # Usage:
  #
  #   * `assign(context, key, value)` - Assigns value to key in context's assigns.
  #   * `assign(context, %{})` - Merges the update map into a context's assigns.
  #   * `assign(context, enumerable)` - Collects the key/values of `enumerable` into a map, then
  #     merges that map into the context's assigns.
  @doc false
  @spec assign(t, any, any) :: t
  def assign(%Context{assigns: assigns} = context, key, value),
    do: %{context | assigns: Map.put(assigns, key, value)}

  @doc false
  @spec assign(t, Enumerable.t()) :: t
  def assign(%Context{assigns: assigns} = context, updates) when is_map(updates),
    do: %{context | assigns: Map.merge(assigns, updates)}

  def assign(context, enum),
    do: assign(context, Enum.into(enum, %{}))

  @doc false
  def actions(context), do: context.private.actions

  @doc false
  def assign_actions(context, actions \\ []),
    do: put_in(context.private.actions, actions)

  @doc false
  def update_actions(context, fun),
    do: update_in(context.private.actions, fun)

  @doc false
  def put_actions(context, actions),
    do: update_actions(context, &(&1 ++ actions))

  @doc false
  def pop_actions(context),
    do: {actions(context), assign_actions(context)}

  @doc false
  def put_reply(context, reply),
    do: update_in(context.private.replies, &[reply | &1])

  @doc false
  def get_replies(context),
    do: context.private.replies |> Enum.reverse()

  @doc false
  def pop_replies(context),
    do: {get_replies(context), put_in(context.private.replies, [])}
end
