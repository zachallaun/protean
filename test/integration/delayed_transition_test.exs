defmodule ProteanIntegration.DelayedTransitionTest do
  use Protean.TestCase, async: true

  defmodule TestMachine do
    use Protean

    @machine [
      assigns: %{
        path: []
      },
      initial: "a",
      states: [
        atomic(:a,
          entry: :save_path,
          after: [
            delay(50, target: "b")
          ]
        ),
        atomic(:b,
          entry: :save_path,
          after: [
            delay(10, target: "c")
          ]
        ),
        atomic(:c,
          entry: :save_path,
          on: [
            {"$protean.after.50-#.a", actions: [:blow_up]}
          ]
        ),
        atomic(:d,
          entry: :save_path
        )
      ],
      on: [
        goto_c: ".c",
        goto_d: ".d"
      ]
    ]

    @impl Protean
    def handle_action(:save_path, %{assigns: %{path: path}} = context, _event) do
      Protean.Action.assign(context, :path, [MapSet.to_list(context.value) | path])
    end

    def handle_action(:blow_up, _context, _event) do
      raise "should never get here!"
    end
  end

  describe "delayed transitions:" do
    @describetag machine: TestMachine

    test "takes automatic delayed transitions", %{machine: machine} do
      assert_protean(machine,
        sleep: 100,
        matches: "c"
      )
    end

    test "delayed transitions can be short-circuited by transitioning early",
         %{machine: machine} do
      assert_protean(machine,
        call: :goto_c,
        sleep: 50,
        matches: "c"
      )
    end

    test "short-circuited transitions don't execute actions", %{machine: machine} do
      assert_protean(machine,
        assigns: [path: [[["a", "#"]]]],
        call: :goto_d,
        sleep: 50,
        assigns: [path: [[["d", "#"]], [["a", "#"]]]]
      )
    end

    test "short-circuited transitions don't still send event", %{machine: machine} do
      assert_protean(machine,
        call: :goto_c,
        sleep: 50
      )
    end
  end

  defmodule DynamicDelay do
    use Protean

    @machine [
      initial: "will_transition",
      states: [
        will_transition: [
          after: [
            delay: "some_delay",
            target: "new_state"
          ]
        ],
        new_state: []
      ]
    ]

    @impl Protean
    def delay("some_delay", _, _), do: 50
  end

  @tag machine: DynamicDelay
  test "delays defined by callback", %{machine: machine} do
    assert_protean(machine,
      matches: "will_transition",
      sleep: 80,
      matches: "new_state"
    )
  end
end
