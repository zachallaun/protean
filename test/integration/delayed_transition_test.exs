defmodule ProteanIntegration.DelayedTransitionTest do
  use Protean.TestCase, async: true

  @moduletag trigger: DelayedTransitionTrigger

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
            delay(10, target: "b", actions: Trigger.action(DelayedTransitionTrigger, :a_after))
          ]
        ),
        atomic(:b,
          entry: [:save_path, Trigger.action(DelayedTransitionTrigger, :b)],
          after: [
            delay(10, target: "c")
          ]
        ),
        atomic(:c,
          entry: [:save_path, Trigger.action(DelayedTransitionTrigger, :c)]
        ),
        atomic(:d,
          entry: [:save_path, Trigger.action(DelayedTransitionTrigger, :d)]
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
  end

  describe "delayed transitions:" do
    @describetag machine: TestMachine

    test "takes automatic delayed transitions", %{machine: machine} do
      Trigger.await(DelayedTransitionTrigger, :c)
      assert Protean.matches?(machine, :c)
    end

    test "delayed transitions can be short-circuited by transitioning early",
         %{machine: machine} do
      Protean.send(machine, :goto_c)
      Trigger.await(DelayedTransitionTrigger, :c)

      assert_protean(machine,
        matches: :c,
        assigns: [path: [[["c", "#"]], [["a", "#"]]]]
      )
    end

    test "short-circuited transitions don't execute actions", %{machine: machine} do
      Protean.call(machine, :goto_d)
      assert Trigger.triggered?(DelayedTransitionTrigger, :d)
      refute Trigger.triggered?(DelayedTransitionTrigger, :b)
      refute Trigger.triggered?(DelayedTransitionTrigger, :c)
    end

    test "short-circuited transitions don't still send event", %{machine: machine} do
      Protean.call(machine, :goto_c)
      :timer.sleep(20)
      refute Trigger.triggered?(DelayedTransitionTrigger, :a_after)
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
        new_state: [
          entry: Trigger.action(DelayedTransitionTrigger, :new_state)
        ]
      ]
    ]

    @impl true
    def delay("some_delay", _, _), do: 10
  end

  @tag machine: DynamicDelay
  test "delays defined by callback", %{machine: machine} do
    assert Protean.matches?(machine, :will_transition)
    assert Trigger.await(DelayedTransitionTrigger, :new_state)
  end
end
