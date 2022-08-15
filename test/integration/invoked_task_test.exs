defmodule ProteanIntegration.InvokedTaskTest do
  use Protean.TestCase, async: true

  @moduletag trigger: InvokedTaskTrigger

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
          invoke: [
            invoked(
              :task,
              fn ->
                :task_result
              end,
              done: [
                actions: ["save_result"],
                target: "b"
              ]
            )
          ]
        ),
        atomic(:b,
          entry: Trigger.action(InvokedTaskTrigger, :b),
          on: [
            goto_c: "c"
          ]
        ),
        atomic(:c,
          invoke: [
            invoked(
              :task,
              fn ->
                :second_task_result
              end,
              done: [
                actions: ["save_result"],
                target: "d"
              ]
            )
          ]
        ),
        atomic(:d, entry: Trigger.action(InvokedTaskTrigger, :d))
      ]
    ]

    @impl true
    def handle_action("save_result", context, {_, result}) do
      Action.assign(context, :result, result)
    end
  end

  describe "AnonymousFunctionTasks:" do
    @describetag machine: AnonymousFunctionTasks

    test "anonymous function task invoked from initial state", %{machine: machine} do
      assert Trigger.await(InvokedTaskTrigger, :b)

      assert_protean(machine,
        assigns: [result: :task_result]
      )
    end

    test "anonymous function task invoked after transition", %{machine: machine} do
      assert Trigger.await(InvokedTaskTrigger, :b)
      Protean.send(machine, :goto_c)
      assert Trigger.await(InvokedTaskTrigger, :d)
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
          invoke: [
            task: {__MODULE__, :my_task, [:arg]},
            done: [
              actions: ["save_result"],
              target: "b"
            ]
          ]
        ],
        b: [
          entry: Trigger.action(InvokedTaskTrigger, :b)
        ]
      ]
    ]

    def my_task(value) do
      {:task_return, value}
    end

    @impl true
    def handle_action("save_result", state, {_, result}) do
      Action.assign(state, :result, result)
    end
  end

  describe "MFATasks:" do
    @describetag machine: MFATasks

    test "MFA task invoked in initial state", %{machine: machine} do
      assert Trigger.await(InvokedTaskTrigger, :b)
      assert %{result: {:task_return, :arg}} = Protean.current(machine).assigns
    end
  end

  defmodule ErrorRaisingTasks do
    use Protean

    @machine [
      initial: "init",
      states: [
        init: [
          invoke: [
            task: {__MODULE__, :raise_error, []},
            done: "success",
            error: "failure"
          ]
        ],
        success: [
          entry: Trigger.action(InvokedTaskTrigger, :success)
        ],
        failure: [
          entry: Trigger.action(InvokedTaskTrigger, :failure)
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
        assert Trigger.await(InvokedTaskTrigger, :failure)
        refute Trigger.triggered?(InvokedTaskTrigger, :success)
      end)
    end
  end

  defmodule ResolvedTaskInvoke do
    use Protean

    @machine [
      initial: "init",
      states: [
        init: [
          invoke: [
            invoked("my_task", done: "success")
          ]
        ],
        success: [
          entry: Trigger.action(InvokedTaskTrigger, :success)
        ]
      ]
    ]

    @impl true
    def invoke("my_task", _state, _event) do
      {:task, fn -> :result end}
    end
  end

  describe "ResolvedTaskInvoke:" do
    @describetag machine: ResolvedTaskInvoke

    test "tasks can be resolved by callback module" do
      assert Trigger.await(InvokedTaskTrigger, :success)
    end
  end

  defmodule CanceledTask do
    use Protean

    @machine [
      initial: "init",
      states: [
        init: [
          invoke: [
            invoked("send_message_to_self", done: "sent")
          ],
          on: [
            cancel: "canceled"
          ]
        ],
        canceled: [
          entry: Trigger.action(InvokedTaskTrigger, :canceled),
          on: [
            message: "sent"
          ]
        ],
        # shouldn't get here
        sent: [
          entry: Trigger.action(InvokedTaskTrigger, :sent)
        ]
      ]
    ]

    @impl true
    def invoke("send_message_to_self", _state, _event) do
      me = self()

      {:task,
       fn ->
         :timer.sleep(30)
         Protean.call(me, "message")
       end}
    end
  end

  describe "CanceledTask:" do
    @describetag machine: CanceledTask

    test "transitioning out of invoking state should cancel task", %{machine: machine} do
      Protean.send(machine, :cancel)
      assert Trigger.await(InvokedTaskTrigger, :canceled)
      refute Trigger.triggered?(InvokedTaskTrigger, :sent)
    end
  end

  defmodule ManyTasks do
    use Protean

    @machine [
      initial: :how_many,
      assigns: %{data: MapSet.new()},
      states: [
        atomic(:how_many,
          invoke: [
            invoked(:one),
            invoked(:two, done: [actions: :save]),
            invoked(
              :task,
              fn ->
                Trigger.trigger(InvokedTaskTrigger, :three)
                :value_three
              end,
              done: [actions: :save]
            )
          ],
          on: [
            match({_, _}, actions: :save)
          ]
        )
      ]
    ]

    @impl true
    def invoke(:one, _, _) do
      {:task,
       fn ->
         Trigger.trigger(InvokedTaskTrigger, :one)
         :value_one
       end}
    end

    def invoke(:two, _, _) do
      {:task,
       fn ->
         Trigger.trigger(InvokedTaskTrigger, :two)
         :value_two
       end}
    end

    @impl true
    def handle_action(:save, state, {_, value}) do
      state
      |> Protean.Action.update_in([:data], &MapSet.put(&1, value))
    end
  end

  @tag machine: ManyTasks
  test "tasks can be invoked in many ways", %{machine: machine} do
    Trigger.await(InvokedTaskTrigger, :one)
    Trigger.await(InvokedTaskTrigger, :two)
    Trigger.await(InvokedTaskTrigger, :three)
    :timer.sleep(5)

    expected = MapSet.new([:value_one, :value_two, :value_three])
    assert %{data: ^expected} = Protean.current(machine).assigns
  end
end
