defmodule Protean.StateNode do
  @moduledoc false

  alias __MODULE__
  alias Protean.Action
  alias Protean.Transition

  @enforce_keys [:id, :type]

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
  A StateNode is a node in a nested state machine. See the type docs for
  individual nodes for more details.
  """
  @type t :: atomic | final | compound | parallel

  @typedoc """
  A leaf StateNode is one that cannot have child states.
  """
  @type leaf :: atomic | final

  @typedoc """
  A simple `StateNode` is one that cannot have child nodes.
  """
  @type simple :: atomic | final

  @typedoc """
  A complex `StateNode` is one that has child nodes.
  """
  @type complex :: compound | parallel

  @typedoc """
  ID encompasses the node and all its ancestors. For example, a node `:child_a`
  defined as a child of a node `:parent_a` might have the id `[:child_a, :parent_a]`
  """
  @opaque id :: [String.t(), ...]

  @typedoc """
  An atomic node is a node without child states.
  """
  @type atomic :: %StateNode{
          type: :atomic,
          id: id,
          initial: nil,
          states: nil,
          automatic_transitions: [Transition.t()],
          transitions: [Transition.t()],
          entry: [Action.t()],
          exit: [Action.t()],
          order: non_neg_integer | nil
        }

  @typedoc """
  A final node is a type of atomic node that represents some form of completion,
  and can therefore define no transitions itself. Note, however, that activating
  a final node causes an event to be dispatched that a parent node can choose to
  handle.
  """
  @type final :: %StateNode{
          type: :final,
          id: id,
          initial: nil,
          states: nil,
          automatic_transitions: [],
          transitions: [],
          entry: [Action.t()],
          exit: [Action.t()],
          order: non_neg_integer | nil
        }

  @typedoc """
  A compound node is a node that defines children, of which only one can be
  active. It must additionally define an `:initial` attribute, the id of the
  child state that should default to active if the compound state is entered.
  """
  @type compound :: %StateNode{
          type: :compound,
          id: id,
          initial: id,
          states: [t, ...],
          automatic_transitions: [Transition.t()],
          transitions: [Transition.t()],
          entry: [Action.t()],
          exit: [Action.t()],
          order: non_neg_integer | nil
        }

  @typedoc """
  A parallel node defines children, all of which are entered when the parallel
  node is entered.
  """
  @type parallel :: %StateNode{
          type: :parallel,
          id: id,
          initial: nil,
          states: [t, ...],
          automatic_transitions: [Transition.t()],
          transitions: [Transition.t()],
          entry: [Action.t()],
          exit: [Action.t()],
          order: non_neg_integer | nil
        }

  @doc """
  Resolve a StateNode to its leaves (atomic or final) by either returning the
  given node or following the node's children.
  """
  @spec resolve_to_leaves(t) :: [leaf]
  def resolve_to_leaves(%StateNode{} = node) do
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

  @doc """
  Given a StateNode id, return a list containing that id and all of its
  ancestors.
  """
  @spec ancestor_ids(id) :: [id]
  def ancestor_ids([]), do: []
  def ancestor_ids([_self | parent] = id), do: [id | ancestor_ids(parent)]

  @doc """
  Tests whether `descendant_id` is in fact a descendant of `ancestor_id`.
  Returns false if they are the same node.
  """
  @spec descendant?(id, id) :: boolean
  def descendant?(descendant_id, ancestor_id) do
    descendant_id != ancestor_id && common_ancestor_id(descendant_id, ancestor_id) == ancestor_id
  end

  # ["a2" "a" "#"]
  # ["a1" "a" "#"]
  #      ["a" "#"]
  # => ["#"]
  #
  # ["#" "a" "a2"]
  # ["#" "a" "a1"]
  # ["#" "a"]
  #
  # truncate to shortest length - 1
  # ["#"]
  # ["#"]
  # ["#"]
  #
  # all same, put in acc: ["#"]
  # all empty, return acc
  #
  #
  #
  # ["d1" "c" "b" "a" "#"]
  # ["d2" "c" "b" "a" "#"]
  #      ["c" "b" "a" "#"]
  #      ["c" "Z" "a" "#"]
  #
  # reverse
  # [# a b c d1]
  # [# a b c d2]
  # [# a b c]
  # [# a Z c]
  #
  # truncate to shortest - 1
  # [# a b]
  # [# a b]
  # [# a b]
  # [# a Z]
  #
  # all same, put in acc: [#]
  # all same, put in acc: [a #]
  # diff, return acc
  #
  # [# a b c d1]
  # [# a b c d2]
  # [# a]
  # [# a b2]
  #
  # acc: []
  # all same, put in acc: ["#"]
  # all same, put in acc: ["a"]
  #
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

  @spec common_ancestor_id(id, id) :: id
  def common_ancestor_id([self | rest], [self | rest]),
    do: List.wrap(rest)

  def common_ancestor_id(id1, id2) do
    [id1, id2]
    |> Enum.map(&Enum.reverse/1)
    |> then(fn [rev1, rev2] -> get_prefix(rev1, rev2) end)
    |> Enum.reverse()
  end

  defp get_prefix([], _), do: []
  defp get_prefix(_, []), do: []
  defp get_prefix([x | xs], [x | ys]), do: [x | get_prefix(xs, ys)]
  defp get_prefix([_x | _xs], [_y | _ys]), do: []
end
