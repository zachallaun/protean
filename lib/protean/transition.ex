defmodule Protean.Transition do
  @moduledoc false

  alias __MODULE__
  alias Protean.{Machine, StateNode}

  defstruct [
    :targets,
    :event_descriptor,
    type: :external
  ]

  @type t :: %Transition{
          event_descriptor: event_descriptor,
          targets: [StateNode.id()],
          type: :internal | :external
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
  @spec enabled?(Transition.t(), Machine.event_name()) :: boolean
  def enabled?(transition, event_name),
    do: event_descriptor_match?(transition.event_descriptor, event_name)

  @doc """
  Gets the target state node ID for the given transition.
  """
  @spec target(Transition.t()) :: StateNode.id()
  def target(%Transition{targets: [target | _ignored_for_now]}), do: target

  @doc """
  Checks whether an event descriptor matches an event name.
  """
  @spec event_descriptor_match?(event_descriptor, Machine.event_name()) :: boolean
  def event_descriptor_match?(descriptor, name) do
    name_parts = String.split(name, ".")
    any_components_match?(descriptor, name_parts)
  end

  defp any_components_match?(descriptor, name_parts),
    do: Enum.any?(descriptor, &component_match?(&1, name_parts))

  defp component_match?([], _name), do: true

  defp component_match?([x | rest_descriptor], [x | rest_name]),
    do: component_match?(rest_descriptor, rest_name)

  defp component_match?(_descriptor, _name), do: false
end
