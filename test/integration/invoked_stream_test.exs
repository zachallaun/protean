defmodule ProteanIntegration.InvokedStreamTest do
  use Protean.TestCase, async: true

  defmodule StreamMachine1 do
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

  @tag machine: StreamMachine1
  test "invoked streams emit events until consumed", %{machine: machine} do
    :timer.sleep(50)

    %{context: context} = state = Protean.current(machine)

    assert length(context[:data]) == 5
    assert Protean.matches?(state, "stream_consumed")
  end

  defmodule StreamMachine2 do
    use Protean
    alias Protean.Action

    @machine [
      initial: "waiting",
      context: [data: []],
      states: [
        waiting: [
          on: [
            stream: "consuming"
          ]
        ],
        consuming: [
          invoke: [
            id: "stream",
            stream: "stream_from_event",
            done: "waiting"
          ],
          on: [
            stream_data: [
              actions: ["write_data"]
            ]
          ]
        ]
      ]
    ]

    @impl true
    def invoke("stream_from_event", _state, {_, stream}) do
      Stream.map(stream, &{"stream_data", &1})
    end

    @impl true
    def action("write_data", state, {_, value}) do
      state
      |> Action.assign_in([:data], &[value | &1])
    end
  end

  @tag machine: StreamMachine2
  test "invoked streams can be resolved by handler", %{machine: machine} do
    assert_protean(machine,
      send: {"stream", Stream.repeatedly(fn -> 1 end) |> Stream.take(5)},
      sleep: 50,
      matches: "waiting",
      context: [data: [1, 1, 1, 1, 1]]
    )
  end
end
