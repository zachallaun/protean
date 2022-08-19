defmodule Protean.ParserTest do
  use ExUnit.Case

  alias Protean.Action
  alias Protean.Events
  alias Protean.Parser

  describe "delayed transition syntax:" do
    test "single transition" do
      id = {["#"], 2000}

      [
        [
          after: [
            delay: 2000,
            target: "b"
          ]
        ],
        [
          entry: [
            Action.spawn(:delayed_send, 2000, id)
          ],
          exit: [
            Action.spawn(:cancel, id)
          ],
          on: [
            {Events.platform(:spawn, :done, id), target: "b"}
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
              guard: "some_condition",
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
            Action.spawn(:delayed_send, 1000, {["#"], 1000}),
            Action.spawn(:delayed_send, 2000, {["#"], 2000})
          ],
          exit: [
            Action.spawn(:cancel, {["#"], 1000}),
            Action.spawn(:cancel, {["#"], 2000})
          ],
          on: [
            {Events.platform(:spawn, :done, {["#"], 1000}), target: "c", guard: "some_condition"},
            {Events.platform(:spawn, :done, {["#"], 2000}), target: "c"}
          ]
        ]
      ]
      |> assert_parsed_same()
    end
  end

  describe "spawn syntax:" do
    test "tasks with anonymous functions" do
      task_fun = fn -> :result end

      [
        [
          spawn: [
            id: "task_id",
            task: task_fun,
            done: "done_state",
            error: "error_state",
            autoforward: true
          ]
        ],
        [
          entry: [
            Action.spawn(:task, task_fun, "task_id", autoforward: true)
          ],
          exit: [
            Action.spawn(:cancel, "task_id")
          ],
          on: [
            {Events.platform(:spawn, :done, "task_id"), target: "done_state"},
            {Events.platform(:spawn, :error, "task_id"), target: "error_state"}
          ]
        ]
      ]
      |> assert_parsed_same()
    end

    test "procs" do
      [
        [
          spawn: [
            id: "proc_id",
            proc: Anything,
            done: "done_state",
            error: "error_state"
          ]
        ],
        [
          entry: [
            Action.spawn(:proc, Anything, "proc_id", autoforward: false)
          ],
          exit: [
            Action.spawn(:cancel, "proc_id")
          ],
          on: [
            {Events.platform(:spawn, :done, "proc_id"), target: "done_state"},
            {Events.platform(:spawn, :error, "proc_id"), target: "error_state"}
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
          {Events.platform(:done, ["#"]), target: "other"}
        ]
      ]
    ]
    |> assert_parsed_same()
  end

  defp assert_parsed_same(nodes) do
    [parsed1, parsed2] = Enum.map(nodes, &Parser.parse_node/1)

    unless parsed_same?(parsed1, parsed2) do
      assert parsed1 == parsed2
    end
  end

  defp parsed_same?(f1, f2) when is_function(f1) and is_function(f2), do: true

  defp parsed_same?(l1, l2) when is_list(l1) and is_list(l2) do
    length(l1) === length(l2) &&
      Enum.all?(Enum.zip(l1, l2), fn {e1, e2} -> parsed_same?(e1, e2) end)
  end

  defp parsed_same?(t1, t2) when is_tuple(t1) and is_tuple(t2) do
    parsed_same?(Tuple.to_list(t1), Tuple.to_list(t2))
  end

  defp parsed_same?(m1, m2) when is_map(m1) and is_map(m2) do
    [l1, l2] =
      for m <- [m1, m2] do
        m |> Map.to_list() |> Enum.sort_by(fn {k, _v} -> k end)
      end

    parsed_same?(l1, l2)
  end

  defp parsed_same?(x, x), do: true
  defp parsed_same?(_, _), do: false
end
