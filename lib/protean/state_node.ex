defmodule Protean.StateNode do
  @moduledoc false

  alias __MODULE__
  alias Protean.{Machine, Transition}

  defstruct [
    :id,
    :type,
    :transitions,
    :states,
    :initial
  ]

  @typedoc """
  A StateNode is a node in a nested state machine. See the type docs for
  individual nodes for more details.
  """
  @type t :: atomic | final | compound

  @typedoc """
  A leaf StateNode is one that cannot have child states.
  """
  @type leaf :: atomic | final

  @typedoc """
  ID encompasses the node and all its ancestors. For example, a node `:child_a`
  defined as a child of a node `:parent_a` might have the id `[:child_a, :parent_a]`
  """
  @type id :: [String.t(), ...]

  @typedoc """
  An atomic node is a node without child states.
  """
  @type atomic :: %StateNode{
          type: :atomic,
          id: id,
          initial: nil,
          states: nil,
          transitions: [Transition.t()] | nil
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
          transitions: nil
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
          states: [StateNode.t(), ...],
          transitions: [Transition.t()] | nil
        }

  @doc """
  Resolve a StateNode to a leaf (atomic or final) node by either returning the
  given node or following the node's `:initial` attribute.
  """
  @spec resolve_to_leaf(StateNode.t()) :: StateNode.leaf()
  def resolve_to_leaf(%StateNode{} = node) do
    case node do
      node when node.type in [:atomic, :final] ->
        node

      %{type: :compound} ->
        node.states
        |> Enum.find(&(&1.id == node.initial))
        |> resolve_to_leaf()
    end
  end

  @doc """
  Given a StateNode id, return a list containing that id and all of its
  ancestors.
  """
  @spec ancestor_ids(StateNode.id()) :: [StateNode.id()]
  def ancestor_ids([]), do: []
  def ancestor_ids([_self | parent] = id), do: [id | ancestor_ids(parent)]

  @doc """
  Given a StateNode and an event, return the first transition defined by that
  node that is enabled by the event, or `nil`.
  """
  @spec enabled_transition(StateNode.t(), Machine.event()) :: Transition.t() | nil
  def enabled_transition(%StateNode{transitions: nil}, _event), do: nil

  def enabled_transition(%StateNode{transitions: transitions}, {event_name, _data}) do
    Enum.find(transitions, &Transition.enabled?(&1, event_name))
  end
end
