defmodule Protean.Action.Choose do
  @moduledoc "TODO"

  alias Protean.Action.Protocol.Resolvable
  alias Protean.Transition.Guard

  defmodule Unresolved do
    @moduledoc false

    defstruct [:actions]

    defimpl Resolvable do
      def resolve(%{actions: actions}, state, handler) do
        choice =
          actions
          |> Enum.find(fn
            {_action, when: guard} -> Guard.allows?(guard, state, state.event, handler)
            _action -> true
          end)
          |> case do
            {action, when: _} -> action
            action -> action
          end

        {nil, choice}
      end
    end
  end
end
