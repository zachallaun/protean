defmodule ProteanIntegration.AutomaticTransitionTest do
  use Protean.TestCase

  defmodule TestMachine do
    use Protean
    alias Protean.Action

    defmachine [
      initial: :a,
      context: %{
        acc: [],
        allow: false
      },
      states: [
        a: [
          on: [
            goto_b: "b"
          ]
        ],
        b: [
          always: [
            target: "c",
            actions: ["auto_to_c"]
          ]
        ],
        c: [
          always: [
            target: "d",
            actions: ["auto_to_d"]
          ]
        ],
        d: [
          always: [
            target: "a",
            when: "allow?"
          ],
          on: [
            allow: [
              target: "e",
              actions: [Action.assign(allow: true)]
            ]
          ]
        ],
        e: []
      ],
      on: [
        goto_a: ".a",
        outer_allow: [
          actions: [Action.assign(allow: true)]
        ]
      ]
    ]

    @impl true
    def action(action_name, %{context: %{acc: acc}} = state, _event) do
      Action.assign(state, :acc, [action_name | acc])
    end

    @impl true
    def condition("allow?", %{context: %{allow: true}}, _event), do: true
  end

  @moduletag machine: TestMachine

  test "automatic transitions trigger actions in correct order", %{machine: machine} do
    assert_protean(machine,
      send: "goto_b",
      matches: "d",
      context: [acc: ["auto_to_d", "auto_to_c"]]
    )
  end

  test "normal transitions take precedence even if auto condition is true", %{machine: machine} do
    assert_protean(machine,
      send: "goto_b",
      send: "allow",
      matches: "e",
      send: "goto_a",
      send: "goto_b",
      matches: "a"
    )
  end

  test "auto transition triggered when condition is met", %{machine: machine} do
    assert_protean(machine,
      send: "goto_b",
      matches: "d",
      send: "outer_allow",
      matches: "a"
    )
  end
end
