defmodule ProteanIntegration.DelayedTransitionTest do
  use Protean.TestCase, async: true

  defmodule TestMachine do
    use Protean

    @machine [
      context: %{
        path: []
      },
      initial: :a,
      states: [
        a: [
          entry: ["save_path"],
          after: [
            delay: 20,
            target: :b
          ]
        ],
        b: [
          entry: ["save_path"],
          after: [
            delay: 20,
            target: :c
          ]
        ],
        c: [
          entry: ["save_path"]
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

    def pure("save_path", state, %{path: path}, _) do
      Protean.Action.assign(state, :path, [state.value | path])
    end
  end

  @tag machine: TestMachine
  test "takes automatic delayed transitions", %{machine: machine} do
    assert Protean.matches?(machine, :a)
    :timer.sleep(20)
    assert Protean.matches?(machine, :b)
    :timer.sleep(20)
    assert Protean.matches?(machine, :c)
  end

  @tag machine: TestMachine
  test "delayed transitions can be short-circuited by transitioning early", %{machine: machine} do
    Protean.send(machine, "goto_c")
    :timer.sleep(20)
    assert Protean.matches?(machine, :c)
  end

  # @tag machine: TestMachine
  @tag skip: true
  test "short-circuited transitions don't execute actions", %{machine: machine} do
    assert [[["a", "#"]]] = Protean.current(machine).context
  end
end
