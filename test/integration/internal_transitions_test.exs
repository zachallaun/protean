defmodule ProteanIntegration.InternalTransitionsTest do
  use Protean.TestCase

  defmodule TestMachine do
    use Protean
    alias Protean.Action

    defmachine [
      context: [
        on_entry: [],
        on_exit: []
      ],
      initial: "a",
      states: [
        a: [
          entry: ["add_a_entry"],
          exit: ["add_a_exit"],
          initial: "a1",
          states: [
            a1: [
              entry: ["add_a1_entry"],
              exit: ["add_a1_exit"],
              on: [
                a1_self_external: [
                  target: "a1",
                  internal: false
                ],
                a1_self_internal: [
                  target: "a1"
                ]
              ]
            ],
            a2: [
              entry: ["add_a2_entry"],
              exit: ["add_a2_exit"]
            ]
          ],
          on: [
            a1_external: [
              target: "a.a1",
              internal: false
            ],
            a1_internal: [
              target: "a.a1"
            ],
            a2_external: [
              target: "a.a2",
              internal: false
            ],
            a2_internal: [
              target: "a.a2",
              internal: true
            ],
            b: [
              target: "b"
            ]
          ]
        ],
        b: [
          on: [
            back_to_a: "a"
          ]
        ]
      ]
    ]

    def action("add_a_entry", state, _), do: add(state, :on_entry, "a")
    def action("add_a1_entry", state, _), do: add(state, :on_entry, "a1")
    def action("add_a2_entry", state, _), do: add(state, :on_entry, "a2")

    def action("add_a_exit", state, _), do: add(state, :on_exit, "a")
    def action("add_a1_exit", state, _), do: add(state, :on_exit, "a1")
    def action("add_a2_exit", state, _), do: add(state, :on_exit, "a2")

    def add(state, key, value) do
      current = state.context[key]
      Action.assign(state, %{key => [value | current]})
    end
  end

  @moduletag machine: TestMachine

  test "internal self-transitions do not trigger entry/exit actions", %{machine: machine} do
    assert_protean(machine,
      send: "a1_self_internal",
      context: [on_entry: ["a1", "a"], on_exit: []]
    )
  end

  test "external self-transitions trigger entry/exit actions", %{machine: machine} do
    assert_protean(machine,
      context: [on_entry: ["a1", "a"], on_exit: []],
      send: "a1_self_external",
      context: [on_entry: ["a1", "a1", "a"], on_exit: ["a1"]]
    )
  end

  test "internal relative transitions do not trigger entry/exit actions", %{machine: machine} do
    assert_protean(machine,
      send: "a2_internal",
      matches: "a.a2",
      context: [on_entry: ["a2", "a1", "a"], on_exit: ["a1"]]
    )
  end

  test "external relative transitions trigger entry/exit actions", %{machine: machine} do
    assert_protean(machine,
      send: "a2_external",
      matches: "a.a2",
      context: [on_entry: ["a2", "a", "a1", "a"], on_exit: ["a", "a1"]]
    )
  end

  test "transition to sibling and back", %{machine: machine} do
    assert_protean(machine,
      send: "b",
      matches: "b",
      context: [on_entry: ["a1", "a"], on_exit: ["a", "a1"]],
      send: "back_to_a",
      matches: "a.a1",
      context: [on_entry: ["a1", "a", "a1", "a"], on_exit: ["a", "a1"]]
    )
  end
end
