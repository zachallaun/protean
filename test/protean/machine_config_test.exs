defmodule Protean.MachineConfigTest do
  use ExUnit.Case

  alias Protean.Action
  alias Protean.MachineConfig

  describe "delayed transition syntax:" do
    test "single transition" do
      [
        [
          after: [
            delay: 2000,
            target: "b"
          ]
        ],
        [
          entry: [
            Action.invoke(:delayed_send, "$protean.after.2000-#", 2000)
          ],
          exit: [
            Action.invoke(:cancel, "$protean.after.2000-#")
          ],
          on: [
            "$protean.after.2000-#": [
              target: "b",
              exact: true
            ]
          ]
        ]
      ]
      |> assert_parsed_same()
    end

    test "multiple transitions with condition" do
      [
        [
          after: [
            [
              delay: 1000,
              when: "some_condition",
              target: "c"
            ],
            [
              delay: 2000,
              target: "c"
            ]
          ]
        ],
        [
          entry: [
            Action.invoke(:delayed_send, "$protean.after.1000-#", 1000),
            Action.invoke(:delayed_send, "$protean.after.2000-#", 2000)
          ],
          exit: [
            Action.invoke(:cancel, "$protean.after.1000-#"),
            Action.invoke(:cancel, "$protean.after.2000-#")
          ],
          on: [
            "$protean.after.1000-#": [
              target: "c",
              when: "some_condition",
              exact: true
            ],
            "$protean.after.2000-#": [
              target: "c",
              exact: true
            ]
          ]
        ]
      ]
      |> assert_parsed_same()
    end
  end

  describe "invoke syntax:" do
    test "tasks with anonymous functions" do
      task_fun = fn -> :result end

      [
        [
          invoke: [
            id: "task_id",
            task: task_fun,
            done: "done_state",
            error: "error_state"
          ]
        ],
        [
          entry: [
            Action.invoke(:task, task_fun, "task_id")
          ],
          exit: [
            Action.invoke(:cancel, "task_id")
          ],
          on: [
            "$protean.invoke.done-task_id": [
              target: "done_state",
              exact: true
            ],
            "$protean.invoke.error-task_id": [
              target: "error_state",
              exact: true
            ]
          ]
        ]
      ]
      |> assert_parsed_same()
    end

    test "procs" do
      [
        [
          invoke: [
            id: "proc_id",
            proc: Anything,
            done: "done_state",
            error: "error_state"
          ]
        ],
        [
          entry: [
            Action.invoke(:proc, Anything, "proc_id")
          ],
          exit: [
            Action.invoke(:cancel, "proc_id")
          ],
          on: [
            "$protean.invoke.done-proc_id": [
              target: "done_state",
              exact: true
            ],
            "$protean.invoke.error-proc_id": [
              target: "error_state",
              exact: true
            ]
          ]
        ]
      ]
      |> assert_parsed_same()
    end
  end

  test "done" do
    [
      [
        initial: "a",
        states: [
          a: []
        ],
        done: "other"
      ],
      [
        initial: "a",
        states: [
          a: []
        ],
        on: [
          "$protean.done-#": [
            target: "other",
            exact: true
          ]
        ]
      ]
    ]
    |> assert_parsed_same()
  end

  defp assert_parsed_same(nodes) do
    [parsed1, parsed2] = Enum.map(nodes, &MachineConfig.parse_node/1)
    assert parsed1 == parsed2
  end
end
