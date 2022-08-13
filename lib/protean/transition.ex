defmodule Protean.Transition do
  @moduledoc """
  TODO

  Event descriptors
  Guards
  Internal
  Exact
  Actions
  """

  alias __MODULE__
  alias Protean.Action
  alias Protean.Context
  alias Protean.Events
  alias Protean.Guard
  alias Protean.Node

  defstruct [
    :source_id,
    :target_ids,
    :match?,
    :guard,
    :domain,
    internal: false,
    actions: []
  ]

  @type t :: %Transition{
          source_id: Node.id(),
          target_ids: [Node.id()] | nil,
          match?: (term() -> boolean()) | term() | nil,
          guard: Guard.t(),
          internal: boolean(),
          actions: [Action.t()],
          domain: Node.id()
        }

  def new(opts \\ []) do
    opts
    |> Keyword.take([:source_id, :target_ids, :match?, :guard, :internal, :actions])
    |> then(&struct(Transition, &1))
    |> with_domain()
  end

  @doc "Return the actions associated with a transition"
  @spec actions(t) :: [Action.t()]
  def actions(%Transition{} = t), do: t.actions

  @doc """
  Checks whether the transition is enabled for the given event.
  """
  @spec enabled?(t, Protean.event() | nil, Context.t(), callback_module :: module()) ::
          as_boolean(term())
  def enabled?(transition, event, context, module) do
    matches?(transition, event) && guard_allows?(transition, context, event, module)
  end

  @spec domain(Transition.t()) :: Node.id()
  def domain(%Transition{} = t), do: t.domain

  defp all_descendants_of?(id, ids) do
    Enum.all?(ids, &Node.descendant?(&1, id))
  end

  defp matches?(%Transition{match?: nil}, _), do: true
  defp matches?(%Transition{match?: match}, %Events.Platform{} = event), do: match == event
  defp matches?(%Transition{match?: match?}, event) when is_function(match?), do: match?.(event)
  defp matches?(%Transition{match?: match}, event), do: match == event

  defp guard_allows?(%Transition{guard: nil}, _, _, _), do: true

  defp guard_allows?(%Transition{guard: guard}, context, event, module) do
    Guard.allows?(guard, context, event, module)
  end

  defp with_domain(%Transition{target_ids: []} = t) do
    %{t | domain: nil}
  end

  defp with_domain(%Transition{target_ids: target_ids, source_id: source_id} = t) do
    if t.internal && all_descendants_of?(source_id, target_ids) do
      %{t | domain: source_id}
    else
      %{t | domain: Node.common_ancestor_id([source_id | target_ids])}
    end
  end
end
