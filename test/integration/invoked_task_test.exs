defmodule ProteanIntegration.InvokedTaskTest do
  use Protean.TestCase, async: true

  defmodule AnonymousFunctionTasks do
    use Protean
    alias Protean.Action

    @machine [
      context: [
        result: nil
      ],
      initial: :a,
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

  describe "AnonymousFunctionTasks" do
    @describetag skip: true
    @describetag machine: AnonymousFunctionTasks

    test "invoke in initial state", %{machine: machine} do
      assert_protean(machine,
        sleep: 20,
        matches: "b",
        context: [result: :task_result]
      )
    end

    test "invoke in state transitioned to", %{machine: machine} do
      assert_protean(machine,
        send: "goto_c",
        sleep: 20,
        matches: "d",
        context: [result: :second_task_result]
      )
    end
  end
end
