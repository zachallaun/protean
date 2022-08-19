defmodule ProteanIntegration.SpawnedTaskTest do
  use Protean.TestCase, async: true

  @moduletag trigger: SpawnedTaskTrigger

  defmodule OneOffTask do
    use Protean

    @machine [
      initial: :init,
      states: [
        atomic(:init,
          spawn: [
            task(fn -> Trigger.trigger(SpawnedTaskTrigger, :ran_task) end)
          ]
        )
      ]
    ]
  end

  describe "OneOffTask:" do
    @describetag machine: OneOffTask

    test "should run" do
      assert Trigger.await(SpawnedTaskTrigger, :ran_task)
    end
  end

  defmodule AnonymousFunctionTasks do
    use Protean
    alias Protean.Action

    @machine [
      assigns: [
        result: nil
      ],
      initial: :a,
      states: [
        atomic(:a,
          spawn: [
            task(fn -> :task_result end,
              done: [
                actions: "save_result",
                target: :b
              ]
            )
          ]
        ),
        atomic(:b,
          entry: Trigger.action(SpawnedTaskTrigger, :b),
          on: [
            goto_c: "c"
          ]
        ),
        atomic(:c,
          spawn: [
            task(fn -> :second_task_result end,
              done: [
                actions: "save_result",
                target: :d
              ]
            )
          ]
        ),
        atomic(:d, entry: Trigger.action(SpawnedTaskTrigger, :d))
      ]
    ]

    @impl true
    def handle_action("save_result", context, result) do
      Action.assign(context, :result, result)
    end
  end

  describe "AnonymousFunctionTasks:" do
    @describetag machine: AnonymousFunctionTasks

    test "anonymous function task spawned from initial state", %{machine: machine} do
      assert Trigger.await(SpawnedTaskTrigger, :b)

      assert_protean(machine,
        assigns: [result: :task_result]
      )
    end

    test "anonymous function task spawned after transition", %{machine: machine} do
      assert Trigger.await(SpawnedTaskTrigger, :b)
      Protean.send(machine, :goto_c)
      assert Trigger.await(SpawnedTaskTrigger, :d)
    end
  end

  defmodule MFATasks do
    use Protean
    alias Protean.Action

    @machine [
      assigns: [result: nil],
      initial: "a",
      states: [
        a: [
          spawn: [
            task({__MODULE__, :my_task, [:arg]},
              done: [
                actions: ["save_result"],
                target: "b"
              ]
            )
          ]
        ],
        b: [
          entry: Trigger.action(SpawnedTaskTrigger, :b)
        ]
      ]
    ]

    def my_task(value) do
      {:task_return, value}
    end

    @impl true
    def handle_action("save_result", state, result) do
      Action.assign(state, :result, result)
    end
  end

  describe "MFATasks:" do
    @describetag machine: MFATasks

    test "MFA task spawned in initial state", %{machine: machine} do
      assert Trigger.await(SpawnedTaskTrigger, :b)
      assert %{result: {:task_return, :arg}} = Protean.current(machine).assigns
    end
  end

  defmodule ErrorRaisingTasks do
    use Protean

    @machine [
      initial: "init",
      states: [
        init: [
          spawn: [
            task({__MODULE__, :raise_error, []},
              done: "success",
              error: "failure"
            )
          ]
        ],
        success: [
          entry: Trigger.action(SpawnedTaskTrigger, :success)
        ],
        failure: [
          entry: Trigger.action(SpawnedTaskTrigger, :failure)
        ]
      ]
    ]

    def raise_error do
      raise "any error"
    end
  end

  describe "ErrorRaisingTasks:" do
    import ExUnit.CaptureLog

    @describetag machine: ErrorRaisingTasks

    test "Error transition taken when task raises" do
      capture_log(fn ->
        assert Trigger.await(SpawnedTaskTrigger, :failure, :infinity)
        refute Trigger.triggered?(SpawnedTaskTrigger, :success)
      end)
    end
  end

  defmodule ResolvedTaskSpawn do
    use Protean

    @machine [
      initial: "init",
      states: [
        init: [
          spawn: [
            task("my_task", done: :success)
          ]
        ],
        success: [
          entry: Trigger.action(SpawnedTaskTrigger, :success)
        ]
      ]
    ]

    @impl true
    def spawn(:task, "my_task", _state, _event) do
      fn -> :result end
    end
  end

  describe "ResolvedTaskspawn:" do
    @describetag machine: ResolvedTaskSpawn

    test "tasks can be resolved by callback module" do
      assert Trigger.await(SpawnedTaskTrigger, :success)
    end
  end

  defmodule CanceledTask do
    use Protean

    @machine [
      initial: "init",
      states: [
        init: [
          spawn: [
            task("send_message_to_self", done: "sent")
          ],
          on: [
            cancel: "canceled"
          ]
        ],
        canceled: [
          entry: Trigger.action(SpawnedTaskTrigger, :canceled),
          on: [
            message: "sent"
          ]
        ],
        # shouldn't get here
        sent: [
          entry: Trigger.action(SpawnedTaskTrigger, :sent)
        ]
      ]
    ]

    @impl true
    def spawn(:task, "send_message_to_self", _state, _event) do
      me = self()

      fn ->
        :timer.sleep(30)
        Protean.call(me, "message")
      end
    end
  end

  describe "CanceledTask:" do
    @describetag machine: CanceledTask

    test "transitioning out of invoking state should cancel task", %{machine: machine} do
      Protean.send(machine, :cancel)
      assert Trigger.await(SpawnedTaskTrigger, :canceled)
      refute Trigger.triggered?(SpawnedTaskTrigger, :sent)
    end
  end

  defmodule ManyTasks do
    use Protean

    @machine [
      initial: :how_many,
      assigns: %{data: MapSet.new()},
      states: [
        atomic(:how_many,
          spawn: [
            task(:one),
            task(:two, done: [actions: :save]),
            task(
              fn ->
                Trigger.trigger(SpawnedTaskTrigger, :three)
                :value_three
              end,
              done: [actions: :save]
            )
          ],
          on: [
            match(_, actions: :save)
          ]
        )
      ]
    ]

    @impl true
    def spawn(:task, :one, _, _) do
      fn ->
        Trigger.trigger(SpawnedTaskTrigger, :one)
        :value_one
      end
    end

    def spawn(:task, :two, _, _) do
      fn ->
        Trigger.trigger(SpawnedTaskTrigger, :two)
        :value_two
      end
    end

    @impl true
    def handle_action(:save, state, value) when not is_nil(value) and is_atom(value) do
      state
      |> Protean.Action.update_in([:data], &MapSet.put(&1, value))
    end

    def handle_action(:save, state, _), do: state
  end

  @tag machine: ManyTasks
  test "tasks can be spawned in many ways", %{machine: machine} do
    Trigger.await(SpawnedTaskTrigger, :one)
    Trigger.await(SpawnedTaskTrigger, :two)
    Trigger.await(SpawnedTaskTrigger, :three)
    :timer.sleep(5)

    expected = MapSet.new([:value_one, :value_two, :value_three])
    assert %{data: ^expected} = Protean.current(machine).assigns
  end
end
