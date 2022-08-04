defmodule ProteanIntegration.InvokedTaskTest do
  use Protean.TestCase, async: true

  defmodule AnonymousFunctionTasks do
    use Protean
    alias Protean.Action

    defmachine(
      context: [
        result: nil
      ],
      initial: "a",
      states: [
        a: [
          invoke: [
            task: fn -> :task_result end,
            done: [
              actions: ["save_result"],
              target: "b"
            ]
          ]
        ],
        b: [
          on: [
            goto_c: "c"
          ]
        ],
        c: [
          invoke: [
            task: fn -> :second_task_result end,
            done: [
              actions: ["save_result"],
              target: "d"
            ]
          ]
        ],
        d: []
      ]
    )

    @impl true
    def action("save_result", state, {_, result}) do
      Action.assign(state, :result, result)
    end
  end

  describe "AnonymousFunctionTasks:" do
    @describetag machine: AnonymousFunctionTasks

    test "anonymous function task invoked from initial state", %{machine: machine} do
      assert_protean(machine,
        sleep: 50,
        matches: "b",
        context: [result: :task_result]
      )
    end

    test "anonymous function task invoked after transition", %{machine: machine} do
      assert_protean(machine,
        sleep: 30,
        call: :goto_c,
        sleep: 30,
        matches: "d",
        context: [result: :second_task_result]
      )
    end
  end

  defmodule MFATasks do
    use Protean
    alias Protean.Action

    defmachine(
      context: [result: nil],
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
        b: []
      ]
    )

    def my_task(value) do
      {:task_return, value}
    end

    @impl true
    def action("save_result", state, {_, result}) do
      Action.assign(state, :result, result)
    end
  end

  describe "MFATasks:" do
    @describetag machine: MFATasks

    test "MFA task invoked in initial state", %{machine: machine} do
      assert_protean(machine,
        sleep: 30,
        matches: "b",
        context: [result: {:task_return, :arg}]
      )
    end
  end

  defmodule ErrorRaisingTasks do
    use Protean

    defmachine(
      initial: "init",
      states: [
        init: [
          invoke: [
            task: {__MODULE__, :raise_error, []},
            done: "success",
            error: "failure"
          ]
        ],
        success: [],
        failure: []
      ]
    )

    def raise_error do
      raise "any error"
    end
  end

  describe "ErrorRaisingTasks:" do
    import ExUnit.CaptureLog

    @describetag machine: ErrorRaisingTasks

    test "Error transition taken when task raises", %{machine: machine} do
      capture_log(fn ->
        assert_protean(machine,
          sleep: 100,
          matches: "failure"
        )
      end)
    end
  end

  defmodule ResolvedTaskInvoke do
    use Protean

    defmachine(
      initial: "init",
      states: [
        init: [
          invoke: [
            task: "my_task",
            done: "success"
          ]
        ],
        success: []
      ]
    )

    @impl true
    def invoke("my_task", _state, _event) do
      fn -> :result end
    end
  end

  describe "ResolvedTaskInvoke:" do
    @describetag machine: ResolvedTaskInvoke

    test "tasks can be resolved by callback module", %{machine: machine} do
      assert_protean(machine,
        sleep: 30,
        matches: "success"
      )
    end
  end

  defmodule CanceledTask do
    use Protean

    defmachine(
      initial: "init",
      states: [
        init: [
          invoke: [
            task: "send_message_to_self",
            done: "sent"
          ],
          on: [
            cancel: "canceled"
          ]
        ],
        canceled: [
          on: [
            message: "sent"
          ]
        ],
        # shouldn't get here
        send: []
      ]
    )

    @impl true
    def invoke("send_message_to_self", _state, _event) do
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
      assert_protean(machine,
        call: :cancel,
        sleep: 50,
        matches: "canceled"
      )
    end
  end
end
