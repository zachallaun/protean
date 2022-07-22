defmodule ProteanIntegration.InvokedTaskTest do
  use Protean.TestCase, async: true

  defmodule AnonymousFunctionTasks do
    use Protean
    alias Protean.Action

    @machine [
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
    ]

    @impl true
    def pure("save_result", %{event: {_, result}} = state, _ctx) do
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
        send: "goto_c",
        sleep: 30,
        matches: "d",
        context: [result: :second_task_result]
      )
    end
  end

  defmodule MFATasks do
    use Protean
    alias Protean.Action

    @machine [
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
    ]

    def my_task(value) do
      {:task_return, value}
    end

    @impl true
    def pure("save_result", %{event: {_, result}} = state, _ctx) do
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
        success: [],
        failure: []
      ]
    ]

    def raise_error do
      raise "any error"
    end
  end

  describe "ErrorRaisingTasks:" do
    import ExUnit.CaptureLog

    @describetag machine: ErrorRaisingTasks

    test "Error transition taken when task raises", %{machine: machine} do
      msg =
        capture_log(fn ->
          assert_protean(machine,
            sleep: 30,
            matches: "failure"
          )
        end)

      assert msg =~ "error"
    end
  end
end
