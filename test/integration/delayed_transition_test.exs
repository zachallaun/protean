defmodule ProteanIntegration.DelayedTransitionTest do
  use Protean.TestCase, async: true

  defmodule TestMachine do
    use Protean

    @machine [
      context: %{
        path: []
      },
      initial: "a",
      states: [
        a: [
          entry: ["save_path"],
          after: [
            delay: 30,
            target: "b"
          ]
        ],
        b: [
          entry: ["save_path"],
          after: [
            delay: 30,
            target: "c"
          ]
        ],
        c: [
          entry: ["save_path"],
          on: [
            "$protean.after.30-#.a": [
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
    def pure("save_path", %{context: %{path: path}} = state, _event) do
      Protean.Action.assign(state, :path, [MapSet.to_list(state.value) | path])
    end

    @impl Protean
    def effect("blow_up", _state, _event) do
      raise "should never get here!"
    end
  end

  @moduletag machine: TestMachine

  test "takes automatic delayed transitions", %{machine: machine} do
    assert_protean(machine,
      sleep: 40,
      matches: "b",
      sleep: 30,
      matches: "c"
    )
  end

  test "delayed transitions can be short-circuited by transitioning early", %{machine: machine} do
    assert_protean(machine,
      send: "goto_c",
      sleep: 40,
      matches: "c"
    )
  end

  test "short-circuited transitions don't execute actions", %{machine: machine} do
    assert_protean(machine,
      context: [path: [[["a", "#"]]]],
      sleep: 40,
      context: [path: [[["b", "#"]], [["a", "#"]]]],
      send: "goto_d",
      sleep: 40,
      context: [path: [[["d", "#"]], [["b", "#"]], [["a", "#"]]]]
    )
  end

  test "short-circuited transitions don't still send event", %{machine: machine} do
    assert_protean(machine,
      send: "goto_c",
      sleep: 40
    )
  end
end
