defmodule ProteanIntegration.InvokedMachineTest do
  use Protean.TestCase, async: true
  import ExUnit.CaptureLog
  alias __MODULE__

  @moduletag trigger: InvokedMachineTrigger

  defmodule Parent do
    use Protean
    alias Protean.Action

    @machine [
      initial: "parenting",
      states: [
        atomic(:parenting,
          invoke: [
            invoked(:proc, InvokedMachineTest.Child, id: "child")
          ],
          on: [
            match(:grow_it, actions: [Action.send(:grow, to: "child")]),
            match({:child_grown, _}, "relax")
          ]
        ),
        atomic(:relax,
          entry: :trigger
        )
      ]
    ]

    @impl true
    def handle_action(:trigger, state, _) do
      Trigger.trigger(InvokedMachineTrigger, {:relax, self()})

      state
    end
  end

  defmodule Child do
    use Protean
    alias Protean.Action

    @machine [
      initial: "growing",
      assigns: %{name: nil},
      states: [
        atomic(:growing,
          on: [
            grow: [
              actions: :send_parent,
              target: "grown"
            ],
            status: [
              actions: Action.send(:still_growing, to: :parent)
            ]
          ]
        ),
        atomic(:grown)
      ]
    ]

    @impl true
    def handle_action(:send_parent, state, _) do
      %{name: name} = state.assigns

      state
      |> Action.send({:child_grown, name}, to: :parent)
    end
  end

  describe "the parent/child relationship" do
    @describetag machine: Parent

    test "sending events between parent/child", %{machine: parent, pid: pid} do
      Protean.send(parent, :grow_it)
      assert Trigger.await(InvokedMachineTrigger, {:relax, pid})
    end
  end

  describe "multiple machines" do
    @describetag machines: [Parent, Parent]

    test "can use same ids for child processes", %{machines: machines} do
      [%{machine: m1, pid: p1}, %{machine: m2, pid: p2}] = machines

      Protean.send(m1, :grow_it)
      Trigger.await(InvokedMachineTrigger, {:relax, p1})

      assert Protean.matches?(m1, :relax)
      refute Protean.matches?(m2, :relax)

      Protean.call(m2, :grow_it)
      Trigger.await(InvokedMachineTrigger, {:relax, p2})

      assert Protean.matches?(m2, :relax)
    end
  end

  defmodule Crashes do
    use Protean

    @machine [
      initial: "can_crash",
      states: [
        can_crash: [
          on: [
            go_boom: [
              actions: ["crash"]
            ]
          ]
        ]
      ]
    ]

    @impl Protean
    def handle_action("crash", _, _) do
      raise "boom"
    end
  end

  defmodule InvokeCrashes do
    use Protean

    @machine [
      initial: "init",
      states: [
        init: [
          invoke: [
            invoked(:proc, ProteanIntegration.InvokedMachineTest.Crashes,
              id: "crashes",
              error: [
                actions: ["save_event"],
                target: "invoke_crashed"
              ]
            )
          ],
          on: [
            make_it_crash: [
              actions: [Protean.Action.send(:go_boom, to: "crashes")]
            ]
          ]
        ],
        invoke_crashed: [
          entry: Trigger.action(InvokedMachineTrigger, :crashed)
        ]
      ]
    ]

    @impl Protean
    def handle_action("save_event", context, event) do
      Protean.Action.assign(context, :crash_event, event)
    end
  end

  describe "invoked machine crashes" do
    @describetag machine: InvokeCrashes

    test "trigger error transition", %{machine: machine} do
      error_message =
        capture_log(fn ->
          Protean.send(machine, :make_it_crash)
          assert Trigger.await(InvokedMachineTrigger, :crashed)
        end)

      assert error_message =~ "boom"
    end
  end

  defmodule ImmediatelyCrashes do
    use Protean

    @machine [
      initial: "crash_now",
      states: [
        crash_now: [
          always: [
            actions: ["crash"]
          ]
        ]
      ]
    ]

    @impl Protean
    def handle_action("crash", _, _) do
      raise "boom"
    end
  end

  defmodule InvokeImmediatelyCrashes do
    use Protean

    @machine [
      initial: "init",
      states: [
        init: [
          invoke: [
            invoked(:proc, ProteanIntegration.InvokedMachineTest.ImmediatelyCrashes,
              error: [
                target: "invoke_crashed"
              ]
            )
          ]
        ],
        invoke_crashed: [
          entry: Trigger.action(InvokedMachineTrigger, :crashed_immediately)
        ]
      ]
    ]
  end

  describe "invoked machine immediately crashes" do
    @describetag machine: InvokeImmediatelyCrashes

    test "trigger error transition" do
      capture_log(fn ->
        assert Trigger.await(InvokedMachineTrigger, :crashed_immediately)
      end)
    end
  end

  defmodule DuplicateIds do
    use Protean

    @machine [
      initial: :outer1,
      assigns: %{log: []},
      states: [
        compound(:outer1,
          initial: :a,
          invoke: [
            invoked(:proc, {InvokedMachineTest.Child, assigns: %{name: "outer1_child"}},
              id: :child,
              error: [
                actions: Trigger.action(InvokedMachineTrigger, :outer1_error)
              ]
            )
          ],
          states: [
            atomic(:a,
              invoke: [
                invoked(:proc, InvokedMachineTest.Child,
                  id: :child,
                  error: [
                    actions: Trigger.action(InvokedMachineTrigger, :inner_error)
                  ]
                )
              ]
            )
          ],
          on: [
            match(:goto_outer2, target: :outer2)
          ]
        ),
        atomic(:outer2,
          invoke: [
            invoked(:proc, {InvokedMachineTest.Child, assigns: %{name: "outer2_child"}},
              id: :child,
              error: [
                actions: Trigger.action(InvokedMachineTrigger, :outer2_error)
              ]
            )
          ]
        )
      ],
      on: [
        match(:grow, actions: Protean.Action.send(:grow, to: :child)),
        match(:status, actions: Protean.Action.send(:status, to: :child)),
        match({:child_grown, _}, actions: :trigger),
        match(:still_growing,
          actions: Trigger.action(InvokedMachineTrigger, :child_growing)
        )
      ]
    ]

    @impl true
    def handle_action(:trigger, state, event) do
      Trigger.trigger(InvokedMachineTrigger, event)
      state
    end
  end

  describe "DuplicateIds:" do
    @describetag machine: DuplicateIds

    test "should take error transition when invoking with duplicate id" do
      assert Trigger.await(InvokedMachineTrigger, :inner_error)
      refute Trigger.triggered?(InvokedMachineTrigger, :outer1_error)
    end

    test "should keep non-errored child accessible", %{machine: machine} do
      Trigger.await(InvokedMachineTrigger, :inner_error)
      Protean.send(machine, :grow)
      assert Trigger.await(InvokedMachineTrigger, {:child_grown, "outer1_child"})
    end

    test "should allow duplicate id after exiting invoke", %{machine: machine} do
      Protean.send(machine, :grow)
      Trigger.await(InvokedMachineTrigger, {:child_grown, "outer1_child"})
      Protean.send(machine, :goto_outer2)
      Protean.send(machine, :status)

      assert Trigger.await(InvokedMachineTrigger, :child_growing)
      Protean.send(machine, :grow)
      assert Trigger.await(InvokedMachineTrigger, {:child_grown, "outer2_child"})
    end
  end
end
