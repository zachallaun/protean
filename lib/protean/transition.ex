defmodule Protean.Transition do
  @moduledoc false

  alias __MODULE__
  alias Protean.{Machine, State, StateNode}

  defstruct [
    :config,
    :event_descriptor,
    type: :external
  ]

  @type t() :: %Transition{
          config: transition_config(),
          event_descriptor: event_descriptor(),
          type: :internal | :external
        }

  @type transition_config() :: [target: State.state_value()]

  @spec from_config(event_descriptor(), transition_config()) :: Transition.t()
  def from_config(descriptor, config) do
    %Transition{event_descriptor: descriptor, config: config}
  end

  @spec target(Transition.t()) :: State.state_value()
  def target(%Transition{config: config}), do: config[:target]

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
  @type event_descriptor :: atom() | String.t() | [[String.t()]]

  @doc """
  Checks whether the transition is enabled for the given event.
  """
  @spec enabled?(Transition.t(), Machine.event_name()) :: boolean()
  def enabled?(transition, event_name),
    do: event_descriptor_match?(transition.event_descriptor, event_name)

  @spec expand_event_descriptor(event_descriptor()) :: event_descriptor()
  def expand_event_descriptor(descriptor)

  # Assume that list descriptors are fully expanded
  def expand_event_descriptor(descriptor) when is_list(descriptor),
    do: descriptor

  def expand_event_descriptor(descriptor) when is_atom(descriptor),
    do: descriptor |> to_string() |> expand_event_descriptor()

  def expand_event_descriptor(descriptor) when is_binary(descriptor) do
    descriptor
    |> String.split(" ")
    |> Enum.map(&String.split(&1, "."))
    |> Enum.map(&expand_descriptor_component/1)
  end

  defp expand_descriptor_component([]), do: []
  defp expand_descriptor_component(["" | rest]), do: expand_descriptor_component(rest)
  defp expand_descriptor_component(["*" | rest]), do: expand_descriptor_component(rest)
  defp expand_descriptor_component([part | rest]), do: [part | expand_descriptor_component(rest)]

  @doc """
  Checks whether an event descriptor matches an event name.
  """
  @spec event_descriptor_match?(event_descriptor(), Machine.event_name()) :: boolean()
  def event_descriptor_match?(descriptor, name) do
    name_parts = String.split(name, ".")

    descriptor
    |> expand_event_descriptor()
    |> any_components_match?(name_parts)
  end

  defp any_components_match?(descriptor_components, name_parts),
    do: Enum.any?(descriptor_components, &component_match?(&1, name_parts))

  defp component_match?([], _name), do: true

  defp component_match?([x | rest_components], [x | rest_name]),
    do: component_match?(rest_components, rest_name)

  defp component_match?(_components, _name), do: false
end
