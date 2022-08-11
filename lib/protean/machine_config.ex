defmodule Protean.MachineConfig do
  @doc """
  Data structure representing a Protean machine.
  """

  alias __MODULE__
  alias Protean.Node
  alias Protean.Parser
  alias Protean.State
  alias Protean.Utils

  @enforce_keys [:id, :root, :default_assigns]

  defstruct [
    :id,
    :root,
    :default_assigns,
    :callback_module,
    idmap: %{}
  ]

  @typedoc "Internal representation of a Protean machine"
  @type t :: %MachineConfig{
          id: binary(),
          root: Node.t(),
          idmap: %{Node.id() => Node.t()},
          default_assigns: State.assigns(),
          callback_module: module()
        }

  @typedoc "User-defined machine configuration."
  @type config :: keyword()

  def new(config, opts \\ []) do
    {root, assigns} = Parser.parse!(config)

    idmap =
      Utils.Tree.reduce(root, %{}, fn node, idmap ->
        {Map.put(idmap, node.id, node), node.states}
      end)

    %MachineConfig{
      id: opts[:id] || Utils.uuid4(),
      root: root,
      default_assigns: assigns,
      idmap: idmap,
      callback_module: opts[:callback_module]
    }
  end

  @doc """
  Fetch a node by its id. Raises if the node cannot be found.
  """
  @spec fetch!(t, Node.id()) :: Node.t()
  def fetch!(%MachineConfig{idmap: idmap}, id), do: Map.fetch!(idmap, id)

  @doc """
  Compute the initial state for a machine configuration, including any entry actions that result
  from entering the default states.
  """
  @spec initial_state(t) :: State.t()
  def initial_state(%MachineConfig{} = config) do
    active_ids =
      config.root
      |> Node.resolve_to_leaves()
      |> Enum.map(& &1.id)

    entry_ids =
      active_ids
      |> Enum.flat_map(&Node.ancestor_ids/1)
      |> Enum.uniq()

    entry_actions =
      entry_ids
      |> Enum.map(&fetch!(config, &1))
      |> Node.entry_order()
      |> Enum.flat_map(& &1.entry)

    State.new(active_ids)
    |> State.assign(config.default_assigns)
    |> State.assign_actions(entry_actions)
  end

  @doc """
  Compute the full set of active nodes for the given states.
  """
  @spec active(t, State.value()) :: MapSet.t(Node.t())
  def active(%MachineConfig{} = config, ids) do
    ids
    |> Enum.map(&fetch!(config, &1))
    |> Enum.flat_map(&Node.resolve_to_leaves/1)
    |> Enum.map(& &1.id)
    |> Enum.flat_map(&lineage(config, &1))
    |> MapSet.new()
  end

  @doc """
  Return the full lineage of the given id, including itself and all of its ancestors.
  """
  @spec lineage(t, Node.id()) :: [Node.t(), ...]
  def lineage(%MachineConfig{} = config, id) do
    id
    |> Node.ancestor_ids()
    |> Enum.map(&fetch!(config, &1))
  end
end
