defmodule Protean.StateNode do
  @moduledoc false

  alias __MODULE__
  alias Protean.{Transition}

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
  ID encompasses the node and all its ancestors. For example, a node `:child_a`
  defined as a child of a node `:parent_a` might have the id `[:child_a, :parent_a]`
  """
  @type id :: [atom, ...]

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
end
