defmodule ProteanIntegration.InvokedStreamTest do
  use Protean.TestCase, async: true

  @moduletag trigger: InvokedStreamTrigger

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
          invoke: [
            invoked(:stream, @stream, done: :stream_consumed)
          ],
          on: [
            match({:stream_data, _}, actions: "write_data")
          ]
        ),
        atomic(:stream_consumed,
          entry: Trigger.action(InvokedStreamTrigger, :stream_consumed)
        )
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
    Trigger.await(InvokedStreamTrigger, :stream_consumed)
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
          invoke: [
            invoked(:stream_from_event,
              done: [
                target: :waiting,
                actions: Trigger.action(InvokedStreamTrigger, :stream_done)
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
    def invoke(:stream_from_event, _context, {_, stream}) do
      {:stream, Stream.map(stream, &{:stream_data, &1})}
    end

    @impl true
    def handle_action("write_data", context, {_, value}) do
      context
      |> Action.assign_in([:data], &[value | &1])
    end
  end

  @tag machine: StreamMachine2
  test "invoked streams can be resolved by callback module", %{machine: machine} do
    Protean.send(machine, {:stream, Stream.repeatedly(fn -> 1 end) |> Stream.take(5)})
    assert Trigger.await(InvokedStreamTrigger, :stream_done)
    assert Protean.matches?(machine, :waiting)
    assert %{data: [1, 1, 1, 1, 1]} = Protean.current(machine).assigns
  end
end
