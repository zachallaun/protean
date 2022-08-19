defmodule ProteanIntegration.SpawnedMachineTest do
  use Protean.TestCase, async: true
  import ExUnit.CaptureLog
  alias __MODULE__

  @moduletag trigger: SpawnedMachineTrigger

  defmodule Parent do
    use Protean
    alias Protean.Action

    @machine [
      initial: "parenting",
      states: [
        atomic(:parenting,
          spawn: [
            proc(SpawnedMachineTest.Child, id: "child")
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
      Trigger.trigger(SpawnedMachineTrigger, {:relax, self()})

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

    test "sending events between parent/child", %{machine: parent} do
      Protean.send(parent, :grow_it)
      assert Trigger.await(SpawnedMachineTrigger, {:relax, parent})
    end
  end

  describe "multiple machines" do
    @describetag machines: [Parent, Parent]

    test "can use same ids for child processes", %{machines: machines} do
      [%{machine: m1}, %{machine: m2}] = machines

      Protean.send(m1, :grow_it)
      Trigger.await(SpawnedMachineTrigger, {:relax, m1})

      assert Protean.matches?(m1, :relax)
      refute Protean.matches?(m2, :relax)

      Protean.call(m2, :grow_it)
      Trigger.await(SpawnedMachineTrigger, {:relax, m2})

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

  defmodule SpawnCrashes do
    use Protean

    @machine [
      initial: "init",
      states: [
        init: [
          spawn: [
            proc(ProteanIntegration.SpawnedMachineTest.Crashes,
              id: "crashes",
              error: [
                actions: ["save_event"],
                target: "spawn_crashed"
              ]
            )
          ],
          on: [
            make_it_crash: [
              actions: [Protean.Action.send(:go_boom, to: "crashes")]
            ]
          ]
        ],
        spawn_crashed: [
          entry: Trigger.action(SpawnedMachineTrigger, :crashed)
        ]
      ]
    ]

    @impl Protean
    def handle_action("save_event", context, event) do
      Protean.Action.assign(context, :crash_event, event)
    end
  end

  describe "spawned machine crashes" do
    @describetag machine: SpawnCrashes

    test "trigger error transition", %{machine: machine} do
      error_message =
        capture_log(fn ->
          Protean.send(machine, :make_it_crash)
          assert Trigger.await(SpawnedMachineTrigger, :crashed)
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

  defmodule SpawnImmediatelyCrashes do
    use Protean

    @machine [
      initial: "init",
      states: [
        init: [
          spawn: [
            proc(ProteanIntegration.SpawnedMachineTest.ImmediatelyCrashes,
              error: [
                target: "spawn_crashed"
              ]
            )
          ]
        ],
        spawn_crashed: [
          entry: Trigger.action(SpawnedMachineTrigger, :crashed_immediately)
        ]
      ]
    ]
  end

  describe "spawn machine immediately crashes" do
    @describetag machine: SpawnImmediatelyCrashes

    test "trigger error transition" do
      capture_log(fn ->
        assert Trigger.await(SpawnedMachineTrigger, :crashed_immediately)
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
          spawn: [
            proc({SpawnedMachineTest.Child, assigns: %{name: "outer1_child"}},
              id: :child,
              error: [
                actions: Trigger.action(SpawnedMachineTrigger, :outer1_error)
              ]
            )
          ],
          states: [
            atomic(:a,
              spawn: [
                proc(SpawnedMachineTest.Child,
                  id: :child,
                  error: [
                    actions: Trigger.action(SpawnedMachineTrigger, :inner_error)
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
          spawn: [
            proc({SpawnedMachineTest.Child, assigns: %{name: "outer2_child"}},
              id: :child,
              error: [
                actions: Trigger.action(SpawnedMachineTrigger, :outer2_error)
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
          actions: Trigger.action(SpawnedMachineTrigger, :child_growing)
        )
      ]
    ]

    @impl true
    def handle_action(:trigger, state, event) do
      Trigger.trigger(SpawnedMachineTrigger, event)
      state
    end
  end

  describe "DuplicateIds:" do
    @describetag machine: DuplicateIds

    test "should take error transition when invoking with duplicate id" do
      assert Trigger.await(SpawnedMachineTrigger, :inner_error)
      refute Trigger.triggered?(SpawnedMachineTrigger, :outer1_error)
    end

    test "should keep non-errored child accessible", %{machine: machine} do
      Trigger.await(SpawnedMachineTrigger, :inner_error)
      Protean.send(machine, :grow)
      assert Trigger.await(SpawnedMachineTrigger, {:child_grown, "outer1_child"})
    end

    test "should allow duplicate id after exiting spawn", %{machine: machine} do
      Protean.send(machine, :grow)
      Trigger.await(SpawnedMachineTrigger, {:child_grown, "outer1_child"})
      Protean.send(machine, :goto_outer2)
      Protean.send(machine, :status)

      assert Trigger.await(SpawnedMachineTrigger, :child_growing)
      Protean.send(machine, :grow)
      assert Trigger.await(SpawnedMachineTrigger, {:child_grown, "outer2_child"})
    end
  end
end
