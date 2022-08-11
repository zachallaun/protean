defmodule ProteanIntegration.InvokedStreamTest do
  use Protean.TestCase, async: true

  defmodule StreamMachine1 do
    use Protean
    alias Protean.Action

    @machine [
      initial: "main",
      assigns: [data: []],
      states: [
        main: [
          invoke: [
            stream:
              fn -> {:stream_data, :rand.uniform()} end
              |> Stream.repeatedly()
              |> Stream.take(5),
            done: "stream_consumed"
          ],
          on: [
            {match({:stream_data, _}), actions: "write_data"}
          ]
        ],
        stream_consumed: []
      ]
    ]

    @impl true
    def handle_action("write_data", context, {_, value}) do
      context
      |> Action.assign_in([:data], &[value | &1])
    end
  end

  @tag machine: StreamMachine1
  test "invoked streams emit events until consumed", %{machine: machine} do
    :timer.sleep(50)

    %{assigns: assigns} = context = Protean.current(machine)

    assert length(assigns[:data]) == 5
    assert Protean.matches?(context, "stream_consumed")
  end

  defmodule StreamMachine2 do
    use Protean
    alias Protean.Action

    @machine [
      initial: "waiting",
      assigns: [data: []],
      states: [
        waiting: [
          on: [
            {match({:stream, _}), "consuming"}
          ]
        ],
        consuming: [
          invoke: [
            stream: "stream_from_event",
            done: "waiting"
          ],
          on: [
            {match({:stream_data, _}), actions: "write_data"}
          ]
        ]
      ]
    ]

    @impl true
    def invoke("stream_from_event", _context, {_, stream}) do
      Stream.map(stream, &{:stream_data, &1})
    end

    @impl true
    def handle_action("write_data", context, {_, value}) do
      context
      |> Action.assign_in([:data], &[value | &1])
    end
  end

  @tag machine: StreamMachine2
  test "invoked streams can be resolved by callback module", %{machine: machine} do
    assert_protean(machine,
      call: {:stream, Stream.repeatedly(fn -> 1 end) |> Stream.take(5)},
      sleep: 50,
      matches: "waiting",
      assigns: [data: [1, 1, 1, 1, 1]]
    )
  end
end
