defmodule ProteanIntegration.SpawnedStreamTest do
  use Protean.TestCase, async: true

  @moduletag trigger: SpawnedStreamTrigger

  defmodule StreamMachine1 do
    use Protean
    alias Protean.Action

    @stream Stream.repeatedly(fn -> {:stream_data, 1} end)
            |> Stream.take(5)

    @machine [
      initial: "main",
      assigns: [data: []],
      states: [
        atomic(:main,
          spawn: [
            stream(@stream, done: :stream_consumed)
          ],
          on: [
            match({:stream_data, _}, actions: "write_data")
          ]
        ),
        atomic(:stream_consumed,
          entry: Trigger.action(SpawnedStreamTrigger, :stream_consumed)
        )
      ]
    ]

    @impl true
    def handle_action("write_data", context, {_, value}) do
      context
      |> Action.update_in([:data], &[value | &1])
    end
  end

  @tag machine: StreamMachine1
  test "spawned streams emit events until consumed", %{machine: machine} do
    Trigger.await(SpawnedStreamTrigger, :stream_consumed)
    %{assigns: assigns} = Protean.current(machine)
    assert length(assigns[:data]) == 5
  end

  defmodule StreamMachine2 do
    use Protean
    alias Protean.Action

    @machine [
      initial: "waiting",
      assigns: [data: []],
      states: [
        atomic(:waiting,
          on: [
            match({:stream, _}, "consuming")
          ]
        ),
        atomic(:consuming,
          spawn: [
            stream(:stream_from_event,
              done: [
                target: :waiting,
                actions: Trigger.action(SpawnedStreamTrigger, :stream_done)
              ]
            )
          ],
          on: [
            match({:stream_data, _}, actions: "write_data")
          ]
        )
      ]
    ]

    @impl true
    def spawn(:stream, :stream_from_event, _context, {_, stream}) do
      Stream.map(stream, &{:stream_data, &1})
    end

    @impl true
    def handle_action("write_data", context, {_, value}) do
      context
      |> Action.update_in([:data], &[value | &1])
    end
  end

  @tag machine: StreamMachine2
  test "spawned streams can be resolved by callback module", %{machine: machine} do
    Protean.send(machine, {:stream, Stream.repeatedly(fn -> 1 end) |> Stream.take(5)})
    assert Trigger.await(SpawnedStreamTrigger, :stream_done)
    assert Protean.matches?(machine, :waiting)
    assert %{data: [1, 1, 1, 1, 1]} = Protean.current(machine).assigns
  end
end
