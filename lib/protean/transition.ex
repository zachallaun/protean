defmodule Protean.Transition do
  @moduledoc false

  alias __MODULE__
  alias Protean.{Machine, State, StateNode, Action}

  defstruct [
    :targets,
    :event_descriptor,
    :guard,
    type: :external,
    actions: []
  ]

  @type t :: %Transition{
          event_descriptor: event_descriptor,
          type: :internal | :external,
          targets: [StateNode.id()] | nil,
          actions: [Action.t()]
        }

  @typedoc """
  An event descriptor is used to match event names based on prefix. The usual
  way to specify a descriptor is in its string or atom form:

      "user.fetch"

  This descriptor would match all of the following event names:

      "user.fetch"
      "user.fetch.SUCCESS"
      "user.fetch.ERROR"

  It would not, however, match:

      "user"
      "user.fetch_success"

  Additionally, a descriptor can match multiple events by separating patterns
  by a space. The following would match both the success and error cases:

      "user.fetch.SUCCESS user.fetch.ERROR"

  Note as well that all of the following match the same set of events:

      "user.fetch"
      "user.fetch."
      "user.fetch.*"

  Internally, atom or string event descriptors are converted into nested lists,
  representing the set of patterns that descriptor would match against. Given
  this, the following two descriptors are the same:

      "user.fetch.SUCCESS user.fetch.ERROR user.other.*"
      # is the same as
      [
        ["user", "fetch", "SUCCESS"],
        ["user", "fetch", "ERROR"],
        ["user", "other"]
      ]
  """
  @type event_descriptor :: [[String.t()]]

  @doc """
  Checks whether the transition is enabled for the given event.
  """
  @spec enabled?(Transition.t(), Machine.event(), State.t(), handler :: module) :: boolean
  def enabled?(transition, {:event, event_name, _} = event, state, handler) do
    responds_to?(transition, event_name) && guard_allows?(transition, event, state, handler)
  end

  @spec responds_to?(Transition.t(), Machine.event_name()) :: boolean
  defp responds_to?(transition, event_name),
    do: event_descriptor_match?(transition.event_descriptor, event_name)

  @doc """
  Gets the target state node IDs for the given transition.
  """
  @spec targets(Transition.t()) :: StateNode.id()
  def targets(%Transition{targets: targets}), do: targets

  def target(%Transition{targets: [target | _]}), do: target
  def target(%Transition{targets: []}), do: nil

  @doc """
  Checks whether an event descriptor matches an event name.
  """
  @spec event_descriptor_match?(event_descriptor, Machine.event_name()) :: boolean
  def event_descriptor_match?(descriptor, name) do
    name_parts = String.split(name, ".")
    any_components_match?(descriptor, name_parts)
  end

  defp guard_allows?(%Transition{guard: nil}, _, _, _), do: true

  defp guard_allows?(%Transition{guard: guard}, event, state, handler) do
    Transition.Guard.allows?(guard, event, state, handler)
  end

  defp any_components_match?(descriptor, name_parts),
    do: Enum.any?(descriptor, &component_match?(&1, name_parts))

  defp component_match?([], _name), do: true

  defp component_match?([x | rest_descriptor], [x | rest_name]),
    do: component_match?(rest_descriptor, rest_name)

  defp component_match?(_descriptor, _name), do: false
end
