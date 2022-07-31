defmodule ProteanIntegration.InvokedStreamTest do
  use Protean.TestCase, async: true

  defmodule TestMachine do
    use Protean
    alias Protean.Action

    @machine [
      initial: "main",
      context: [data: []],
      states: [
        main: [
          invoke: [
            id: "mystream",
            stream:
              fn -> {"stream_data", :rand.uniform()} end
              |> Stream.repeatedly()
              |> Stream.take(5),
            done: "stream_consumed"
          ],
          on: [
            stream_data: [
              actions: ["write_data"]
            ]
          ]
        ],
        stream_consumed: []
      ]
    ]

    @impl true
    def action("write_data", state, {_, value}) do
      state
      |> Action.assign_in([:data], &[value | &1])
    end
  end

  @tag machine: TestMachine
  test "invoked streams emit events until consumed", %{machine: machine} do
    :timer.sleep(50)

    %{context: context} = state = Protean.current(machine)

    assert length(context[:data]) == 5
    assert Protean.matches?(state, "stream_consumed")
  end
end
