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
      assigns: [
        received: []
      ],
      states: [
        atomic(:waiting,
          on: [
            {"listen", "listening"}
          ]
        ),
        atomic(:listening,
          on: [
            match({"ping", _}, target: "ponged", actions: "reply"),
            match({"pong", _}, target: "pinged", actions: "reply")
          ]
        ),
        atomic(:pinged,
          on: [
            match({"pong", _}, target: "waiting", actions: "reply")
          ]
        ),
        atomic(:ponged,
          on: [
            match({"ping", _}, target: "waiting", actions: "reply")
          ]
        )
      ]
    ]

    @impl true
    def handle_action("reply", context, {ping_or_pong, from}) do
      %{assigns: %{received: received}} = context
      reply = if ping_or_pong == "ping", do: "pong", else: "ping"

      context
      |> Action.assign(received: [ping_or_pong | received])
      |> Action.send({reply, self()}, to: from)
    end
  end

  @moduletag machines: [PingPong, PingPong]

  test "PingPong", %{machines: machines} do
    [%{machine: m1}, %{machine: m2}] = machines

    Protean.call(m1, "listen")
    Protean.call(m2, "listen")

    assert_protean(m1,
      call: {"ping", m2},
      sleep: 10,
      assigns: [received: ["ping", "ping"]]
    )

    assert_protean(m2,
      assigns: [received: ["pong", "pong"]]
    )

    Protean.call(m1, "listen")
    Protean.call(m2, "listen")

    assert_protean(m2,
      call: {"ping", m1},
      sleep: 10,
      assigns: [received: ["ping", "ping", "pong", "pong"]]
    )

    assert_protean(m1,
      assigns: [received: ["pong", "pong", "ping", "ping"]]
    )
  end
end
