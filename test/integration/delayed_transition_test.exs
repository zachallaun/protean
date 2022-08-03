defmodule ProteanIntegration.DelayedTransitionTest do
  use Protean.TestCase, async: true

  defmodule TestMachine do
    use Protean

    defmachine [
      context: %{
        path: []
      },
      initial: "a",
      states: [
        a: [
          entry: ["save_path"],
          after: [
            delay: 50,
            target: "b"
          ]
        ],
        b: [
          entry: ["save_path"],
          after: [
            delay: 10,
            target: "c"
          ]
        ],
        c: [
          entry: ["save_path"],
          on: [
            "$protean.after.50-#.a": [
              actions: ["blow_up"]
            ]
          ]
        ],
        d: [
          entry: ["save_path"]
        ]
      ],
      on: [
        goto_c: ".c",
        goto_d: ".d"
      ]
    ]

    @impl Protean
    def action("save_path", %{context: %{path: path}} = state, _event) do
      Protean.Action.assign(state, :path, [MapSet.to_list(state.value) | path])
    end

    def action("blow_up", _state, _event) do
      raise "should never get here!"
    end
  end

  @moduletag machine: TestMachine

  test "takes automatic delayed transitions", %{machine: machine} do
    assert_protean(machine,
      sleep: 100,
      matches: "c"
    )
  end

  test "delayed transitions can be short-circuited by transitioning early", %{machine: machine} do
    assert_protean(machine,
      send: "goto_c",
      sleep: 50,
      matches: "c"
    )
  end

  test "short-circuited transitions don't execute actions", %{machine: machine} do
    assert_protean(machine,
      context: [path: [[["a", "#"]]]],
      send: "goto_d",
      sleep: 50,
      context: [path: [[["d", "#"]], [["a", "#"]]]]
    )
  end

  test "short-circuited transitions don't still send event", %{machine: machine} do
    assert_protean(machine,
      send: "goto_c",
      sleep: 50
    )
  end
end
