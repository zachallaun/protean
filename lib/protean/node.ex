defmodule Protean.Node do
  @moduledoc """
  Internal representation of an individual node in a `Protean.MachineConfig` struct.
  """

  alias __MODULE__
  alias Protean.Action
  alias Protean.Transition

  @enforce_keys [:id, :type]

  @derive {Inspect, only: [:id, :type, :initial, :order, :states]}
  defstruct [
    :id,
    :type,
    :initial,
    :states,
    :order,
    automatic_transitions: [],
    transitions: [],
    entry: [],
    exit: []
  ]

  @typedoc """
  A Node is a node in a nested state machine. See the type docs for
  individual nodes for more details.
  """
  @type t :: atomic | final | compound | parallel

  @type nodes :: [t]

  @typedoc """
  A leaf Node is one that cannot have child states.
  """
  @type leaf :: atomic | final

  @typedoc """
  A simple `Node` is one that cannot have child nodes.
  """
  @type simple :: atomic | final

  @typedoc """
  A complex `Node` is one that has child nodes.
  """
  @type complex :: compound | parallel

  @typedoc """
  ID encompasses the node and all its ancestors. For example, a node `:child_a`
  defined as a child of a node `:parent_a` might have the id `[:child_a, :parent_a]`
  """
  @type id :: [String.t(), ...]

  @typedoc """
  An atomic node is a node without child states.
  """
  @type atomic :: %Node{
          type: :atomic,
          id: id,
          initial: nil,
          states: nil,
          automatic_transitions: [Transition.t()],
          transitions: [Transition.t()],
          entry: [Action.t()],
          exit: [Action.t()],
          order: non_neg_integer() | nil
        }

  @typedoc """
  A final node is a type of atomic node that represents some form of completion,
  and can therefore define no transitions itself. Note, however, that activating
  a final node causes an event to be dispatched that a parent node can choose to
  handle.
  """
  @type final :: %Node{
          type: :final,
          id: id,
          initial: nil,
          states: nil,
          automatic_transitions: [],
          transitions: [],
          entry: [Action.t()],
          exit: [Action.t()],
          order: non_neg_integer() | nil
        }

  @typedoc """
  A compound node is a node that defines children, of which only one can be
  active. It must additionally define an `:initial` attribute, the id of the
  child state that should default to active if the compound state is entered.
  """
  @type compound :: %Node{
          type: :compound,
          id: id,
          initial: id,
          states: [t, ...],
          automatic_transitions: [Transition.t()],
          transitions: [Transition.t()],
          entry: [Action.t()],
          exit: [Action.t()],
          order: non_neg_integer() | nil
        }

  @typedoc """
  A parallel node defines children, all of which are entered when the parallel
  node is entered.
  """
  @type parallel :: %Node{
          type: :parallel,
          id: id,
          initial: nil,
          states: [t, ...],
          automatic_transitions: [Transition.t()],
          transitions: [Transition.t()],
          entry: [Action.t()],
          exit: [Action.t()],
          order: non_neg_integer() | nil
        }

  @doc "Return the entry actions associated with the node."
  @spec entry_actions(t) :: [Action.t()]
  def entry_actions(%Node{} = n), do: n.entry

  @doc "Return the exit actions associated with the node."
  @spec exit_actions(t) :: [Action.t()]
  def exit_actions(%Node{} = n), do: n.exit

  @doc """
  Resolve a Node to its leaves (atomic or final) by either returning the
  given node or following the node's children.
  """
  @spec resolve_to_leaves(t) :: [leaf]
  def resolve_to_leaves(%Node{} = node) do
    case node.type do
      :atomic ->
        [node]

      :final ->
        [node]

      :compound ->
        node.states
        |> Enum.find(&(&1.id == node.initial))
        |> resolve_to_leaves()

      :parallel ->
        node.states
        |> Enum.flat_map(&resolve_to_leaves/1)
    end
  end

  def leaf?(%Node{type: :atomic}), do: true
  def leaf?(%Node{type: :final}), do: true
  def leaf?(_), do: false

  @doc "Given a Node id, return a list containing that id and all of its ancestors."
  @spec ancestor_ids(id) :: [id]
  def ancestor_ids([]), do: []
  def ancestor_ids([_self | parent] = id), do: [id | ancestor_ids(parent)]

  @doc "Given a Node id, return its parent's id. Returns nil if the Node is the root."
  @spec parent_id(id) :: id | nil
  def parent_id([_]), do: nil
  def parent_id([_ | parent]), do: parent

  @doc "Sort the given nodes in entry order."
  @spec entry_order(nodes) :: nodes
  def entry_order(nodes) do
    Enum.sort_by(nodes, & &1.order, :asc)
  end

  @doc "Sort the given nodes in exit order."
  @spec exit_order(nodes) :: nodes
  def exit_order(nodes) do
    Enum.sort_by(nodes, & &1.order, :desc)
  end

  @doc """
  Tests whether `descendant_id` is in fact a descendant of `ancestor_id`.
  Returns false if they are the same node.
  """
  @spec descendant?(id, id) :: boolean()
  def descendant?(descendant_id, ancestor_id) do
    descendant_id != ancestor_id &&
      common_ancestor_id(descendant_id, ancestor_id) == ancestor_id
  end

  @spec common_ancestor_id(id, id) :: id
  def common_ancestor_id(id1, id2) do
    [id1, id2]
    |> Enum.map(&Enum.reverse/1)
    |> then(fn [rev1, rev2] -> get_prefix(rev1, rev2) end)
    |> Enum.reverse()
  end

  def common_ancestor_id(ids) do
    shortest = ids |> Enum.map(&Enum.count/1) |> Enum.min()

    ids
    |> Enum.map(&Enum.reverse/1)
    |> Enum.map(&Enum.take(&1, shortest - 1))
    |> do_common_ancestor_id()
  end

  defp do_common_ancestor_id(ids, acc \\ [])

  defp do_common_ancestor_id([[] | _], acc), do: acc

  defp do_common_ancestor_id(ids, acc) do
    [id | _] = ids

    if Enum.all?(ids, &(hd(&1) == hd(id))) do
      ids
      |> Enum.map(&Enum.drop(&1, 1))
      |> do_common_ancestor_id([hd(id) | acc])
    else
      acc
    end
  end

  defp get_prefix([], _), do: []
  defp get_prefix(_, []), do: []
  defp get_prefix([x | xs], [x | ys]), do: [x | get_prefix(xs, ys)]
  defp get_prefix([_x | _xs], [_y | _ys]), do: []
end
