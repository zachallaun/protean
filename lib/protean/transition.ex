defmodule Protean.Transition do
  @moduledoc false

  alias __MODULE__
  alias Protean.Action
  alias Protean.Machine
  alias Protean.State
  alias Protean.Node

  defstruct [
    :source_id,
    :target_ids,
    :event_descriptor,
    :guard,
    internal: false,
    actions: []
  ]

  @type t :: %Transition{
          source_id: Node.id(),
          target_ids: [Node.id()] | nil,
          event_descriptor: event_descriptor,
          guard: Transition.Guard.guard(),
          internal: boolean,
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
  @spec enabled?(t, Machine.event() | nil, State.t(), handler :: module) :: boolean
  def enabled?(transition, event, state, handler) do
    responds_to?(transition, event) && guard_allows?(transition, state, event, handler)
  end

  @spec responds_to?(t, Machine.event() | nil) :: boolean
  defp responds_to?(%Transition{event_descriptor: nil}, _event), do: true

  defp responds_to?(transition, {event_name, _}),
    do: event_descriptor_match?(transition.event_descriptor, event_name)

  @doc """
  Checks whether an event descriptor matches an event name.
  """
  @spec event_descriptor_match?(event_descriptor, Machine.event_name()) :: boolean
  def event_descriptor_match?(descriptor, name) do
    name_parts = String.split(name, ".")
    any_components_match?(descriptor, name_parts)
  end

  defp guard_allows?(%Transition{guard: nil}, _, _, _), do: true

  defp guard_allows?(%Transition{guard: guard}, state, event, handler) do
    Transition.Guard.allows?(guard, state, event, handler)
  end

  defp any_components_match?(descriptor, name_parts),
    do: Enum.any?(descriptor, &component_match?(&1, name_parts))

  defp component_match?([], _name), do: true

  defp component_match?([x | rest_descriptor], [x | rest_name]),
    do: component_match?(rest_descriptor, rest_name)

  defp component_match?(_descriptor, _name), do: false
end
