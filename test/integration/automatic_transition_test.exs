defmodule ProteanIntegration.AutomaticTransitionTest do
  use Protean.TestCase, async: true

  defmodule TestMachine do
    use Protean
    alias Protean.Action

    @machine [
      initial: :a,
      assigns: %{
        acc: [],
        allow: false
      },
      states: [
        atomic(:a,
          on: [
            goto_b: "b"
          ]
        ),
        atomic(:b,
          always: [
            target: "c",
            actions: ["auto_to_c"]
          ]
        ),
        atomic(:c,
          always: [
            target: "d",
            actions: ["auto_to_d"]
          ]
        ),
        atomic(:d,
          always: [
            target: "a",
            guard: "allow?"
          ],
          on: [
            allow: [
              target: "e",
              actions: [Action.assign(allow: true)]
            ]
          ]
        ),
        atomic(:e)
      ],
      on: [
        goto_a: ".a",
        outer_allow: [
          actions: [Action.assign(allow: true)]
        ]
      ]
    ]

    @impl true
    def handle_action(action_name, %{assigns: %{acc: acc}} = context, _event) do
      Action.assign(context, :acc, [action_name | acc])
    end

    @impl true
    def guard("allow?", %{assigns: %{allow: true}}, _event), do: true
  end

  @moduletag machine: TestMachine

  test "automatic transitions trigger actions in correct order", %{machine: machine} do
    assert_protean(machine,
      call: :goto_b,
      matches: "d",
      assigns: [acc: ["auto_to_d", "auto_to_c"]]
    )
  end

  test "normal transitions take precedence even if auto condition is true", %{machine: machine} do
    assert_protean(machine,
      call: :goto_b,
      call: :allow,
      matches: "e",
      call: :goto_a,
      call: :goto_b,
      matches: "a"
    )
  end

  test "auto transition triggered when condition is met", %{machine: machine} do
    assert_protean(machine,
      call: :goto_b,
      matches: "d",
      call: :outer_allow,
      matches: "a"
    )
  end
end
