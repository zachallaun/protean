defmodule ProteanIntegration.InternalTransitionsTest do
  use Protean.TestCase, async: true

  defmodule Siblings do
    use Protean
    alias Protean.Action

    @machine [
      assigns: [
        on_entry: [],
        on_exit: []
      ],
      initial: "a",
      states: [
        compound(:a,
          initial: "a1",
          entry: ["add_a_entry"],
          exit: ["add_a_exit"],
          states: [
            atomic(:a1,
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
            ),
            atomic(:a2,
              entry: ["add_a2_entry"],
              exit: ["add_a2_exit"]
            )
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
        ),
        atomic(:b,
          on: [
            back_to_a: "a"
          ]
        )
      ]
    ]

    def handle_action("add_a_entry", context, _), do: add(context, :on_entry, "a")
    def handle_action("add_a1_entry", context, _), do: add(context, :on_entry, "a1")
    def handle_action("add_a2_entry", context, _), do: add(context, :on_entry, "a2")

    def handle_action("add_a_exit", context, _), do: add(context, :on_exit, "a")
    def handle_action("add_a1_exit", context, _), do: add(context, :on_exit, "a1")
    def handle_action("add_a2_exit", context, _), do: add(context, :on_exit, "a2")

    def add(context, key, value) do
      current = context.assigns[key]
      Action.assign(context, %{key => [value | current]})
    end
  end

  describe "sibling states" do
    @describetag machine: Siblings

    test "internal self-transitions do not trigger entry/exit actions", %{machine: machine} do
      assert_protean(machine,
        call: :a1_self_internal,
        assigns: [on_entry: ["a1", "a"], on_exit: []]
      )
    end

    test "external self-transitions trigger entry/exit actions", %{machine: machine} do
      assert_protean(machine,
        assigns: [on_entry: ["a1", "a"], on_exit: []],
        call: :a1_self_external,
        assigns: [on_entry: ["a1", "a1", "a"], on_exit: ["a1"]]
      )
    end

    test "internal relative transitions do not trigger entry/exit actions", %{machine: machine} do
      assert_protean(machine,
        call: :a2_internal,
        matches: "a.a2",
        assigns: [on_entry: ["a2", "a1", "a"], on_exit: ["a1"]]
      )
    end

    test "external relative transitions trigger entry/exit actions", %{machine: machine} do
      assert_protean(machine,
        call: :a2_external,
        matches: "a.a2",
        assigns: [on_entry: ["a2", "a", "a1", "a"], on_exit: ["a", "a1"]]
      )
    end

    test "transition to sibling and back", %{machine: machine} do
      assert_protean(machine,
        call: :b,
        matches: "b",
        assigns: [on_entry: ["a1", "a"], on_exit: ["a", "a1"]],
        call: :back_to_a,
        matches: "a.a1",
        assigns: [on_entry: ["a1", "a", "a1", "a"], on_exit: ["a", "a1"]]
      )
    end
  end

  defmodule Parents do
    use Protean

    @machine [
      initial: "parent",
      states: [
        parent: [
          initial: "a",
          on: [
            {:parent_internal, actions: [:noop]},
            {:parent_external, actions: [:noop], internal: false}
          ],
          states: [
            a: [
              on: [
                {:goto_b, target: "b"}
              ]
            ],
            b: []
          ]
        ]
      ]
    ]

    @impl Protean
    def handle_action(:noop, context, _), do: context
  end

  describe "parent transitions" do
    @describetag machine: Parents

    test "internal parent transition does not change active children", %{machine: machine} do
      assert_protean(machine,
        matches: "parent.a",
        call: :goto_b,
        matches: "parent.b",
        call: :parent_internal,
        matches: "parent.b"
      )
    end

    test "external parent transitions re-enter children", %{machine: machine} do
      assert_protean(machine,
        matches: "parent.a",
        call: :goto_b,
        matches: "parent.b",
        call: :parent_external,
        matches: "parent.a"
      )
    end
  end
end
