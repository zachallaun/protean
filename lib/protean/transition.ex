defmodule Protean.Transition do
  @moduledoc false

  alias __MODULE__
  alias Protean.{Machine, State, StateNode, Action}

  defstruct [:config, :event, :source, :actions, :type]

  @type t() :: %Transition{
          config: transition_config(),
          event: Machine.event(),
          source: StateNode.t(),
          actions: [Action.t()],
          type: :internal | :external
        }

  @type transition_config() :: [target: State.state_value()]

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
  An event descriptor
  """
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
end
