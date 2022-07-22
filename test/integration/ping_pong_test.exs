defmodule ProteanIntegration.PingPongTest do
  use Protean.TestCase

  defmodule PingPong do
    use Protean
    alias Protean.Action

    @doc """
    A ping pong machine that waits for a {"ping", pid} or {"pong", pid} and replies with the
    opposite. It then waits for one more cycle
    """
    @machine [
      initial: "waiting",
      context: [
        received: []
      ],
      states: [
        waiting: [
          on: [
            listen: "listening"
          ]
        ],
        listening: [
          on: [
            ping: [
              actions: ["reply"],
              target: "ponged"
            ],
            pong: [
              actions: ["reply"],
              target: "pinged"
            ]
          ]
        ],
        pinged: [
          on: [
            pong: [
              actions: ["reply"],
              target: "waiting"
            ]
          ]
        ],
        ponged: [
          on: [
            ping: [
              actions: ["reply"],
              target: "waiting"
            ]
          ]
        ]
      ]
    ]

    @impl true
    def pure("reply", state, {ping_or_pong, from}) do
      %{context: %{received: received}} = state
      reply = if ping_or_pong == "ping", do: "pong", else: "ping"

      state
      |> Action.assign(received: [ping_or_pong | received])
      |> Action.send_event({reply, self()}, to: from)
    end
  end

  @moduletag machines: [PingPong, PingPong]

  test "PingPong", %{machines: machines} do
    [%{machine: m1}, %{machine: m2}] = machines

    Protean.send_event(m1, "listen")
    Protean.send_event(m2, "listen")

    assert_protean(m1,
      send: {"ping", m2},
      sleep: 10,
      context: [received: ["ping", "ping"]]
    )

    assert_protean(m2,
      context: [received: ["pong", "pong"]]
    )

    Protean.send_event(m1, "listen")
    Protean.send_event(m2, "listen")

    assert_protean(m2,
      send: {"ping", m1},
      sleep: 10,
      context: [received: ["ping", "ping", "pong", "pong"]]
    )

    assert_protean(m1,
      context: [received: ["pong", "pong", "ping", "ping"]]
    )
  end
end
