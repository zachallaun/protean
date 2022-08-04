defmodule ProteanIntegration.AskAnswerTest do
  use Protean.TestCase

  defmodule EveryOther do
    use Protean
    alias Protean.Action

    defmachine(
      initial: "Yield",
      states: [
        {
          "Yield",
          on: [
            {:foo, target: "Skip", actions: :answer}
          ]
        },
        {
          "Skip",
          on: [
            {_, target: "Yield"}
          ]
        }
      ]
    )

    @impl true
    def action(:answer, state, event) do
      state
      |> Action.answer(event)
    end
  end

  describe "EveryOther machine:" do
    @describetag machine: EveryOther

    test "yields an answer every other event", %{machine: machine} do
      assert_protean(machine,
        ask: {:foo, {:ok, :foo}},
        ask: {:foo, nil},
        ask: {:foo, {:ok, :foo}},
        ask: {:foo, nil}
      )
    end
  end
end
