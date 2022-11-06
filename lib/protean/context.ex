defprotocol Protean.Context.Store do
  @typedoc "Any type that implements the `Protean.Context.Store` protocol."
  @type t :: any()

  @doc "Get the `%Protean.Context{}` stored in the given data structure."
  @spec get_context(t) :: Protean.Context.t() | nil
  def get_context(t)

  @doc "Put a new `%Protean.Context{}` in the given data structure."
  @spec put_context(t, Protean.Context.t()) :: t
  def put_context(t, ctx)
end

defimpl Protean.Context.Store, for: Map do
  @key :__protean__

  def get_context(m), do: Map.get(m, @key, nil)
  def put_context(m, ctx), do: Map.put(m, @key, ctx)
end

defmodule Protean.Context do
  @moduledoc """
  State available to observers of and callbacks in a machine.

  Functions in this module should rarely be used directly. Instead, rely on the API exposed by
  `Protean` and `Protean.Action` to query and modify machine context.
  """

  alias __MODULE__
  alias __MODULE__.Store
  alias Protean.Action
  alias Protean.Node

  defstruct [
    :id,
    :value,
    :event,
    :parent,
    :config,
    final: MapSet.new(),
    running: false,
    internal_queue: :queue.new(),
    hooks: %{},
    actions: [],
    replies: []
  ]

  @type t :: %Context{
          id: Protean.id() | nil,
          value: value,
          event: Protean.event() | nil,
          parent: pid(),
          config: MachineConfig.t(),
          final: value,
          running: boolean(),
          internal_queue: :queue.queue(),
          hooks: %{},
          actions: [Action.t()],
          replies: [any()]
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

  @doc false
  @spec fetch(Store.t(), atom()) :: any()
  def fetch(store, key) do
    store
    |> Store.get_context()
    |> Map.fetch(key)
  end

  @doc false
  @spec get(Store.t(), atom(), any()) :: any()
  def get(store, key, default \\ nil) do
    case fetch(store, key) do
      {:ok, value} -> value
      :error -> default
    end
  end

  @doc false
  @spec put(Store.t(), atom(), any()) :: Store.t()
  def put(store, key, value) do
    ctx =
      store
      |> Store.get_context()
      |> Map.put(key, value)

    Store.put_context(store, ctx)
  end

  @doc false
  @spec update(Store.t(), atom(), (any() -> any())) :: Store.t()
  def update(store, key, func) when is_function(func, 1) do
    value = store |> get(key) |> func.()
    put(store, key, value)
  end

  @doc """
  See `Protean.matches?/2`.
  """
  @spec matches?(Store.t(), Node.id() | String.t() | atom()) :: boolean()
  def matches?(store, descriptor) do
    store
    |> Store.get_context()
    |> context_matches?(descriptor)
  end

  defp context_matches?(%Context{value: value}, query) when is_list(query) do
    Enum.any?(value, fn id -> id == query || Node.descendant?(id, query) end)
  end

  defp context_matches?(context, query) when is_binary(query) do
    matches?(context, parse_match_query(query))
  end

  defp context_matches?(context, query) when is_atom(query) do
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
  def assign_active(store, ids) do
    put(store, :value, MapSet.new(ids))
  end

  @doc false
  @spec assign_final(Store.t(), MapSet.t(Node.id())) :: t
  def assign_final(store, ids) do
    put(store, :final, ids)
  end

  @doc false
  def assign_actions(store, actions \\ []) do
    put(store, :actions, actions)
  end

  @doc false
  def update_actions(store, func) do
    update(store, :actions, func)
  end

  @doc false
  def append_actions(store, actions) do
    update(store, :actions, &(&1 ++ actions))
  end

  @doc false
  def pop_actions(%Context{actions: actions} = ctx) do
    {actions, %{ctx | actions: []}}
  end

  @doc false
  def pop_actions(store) do
    {get(store, :actions), put(store, :actions, [])}
  end

  @doc false
  def add_reply(store, reply) do
    update(store, :replies, &[reply | &1])
  end

  @doc false
  def get_replies(store) do
    get(store, :replies)
  end

  @doc false
  def pop_replies(store) do
    {get(store, :replies), put(store, :replies, [])}
  end

  @doc false
  def add_internal(store, event) do
    update(store, :internal_queue, &:queue.in(event, &1))
  end

  @doc false
  def add_many_internal(store, [event | rest]) do
    store
    |> add_internal(event)
    |> add_many_internal(rest)
  end

  def add_many_internal(store, []), do: store
end
