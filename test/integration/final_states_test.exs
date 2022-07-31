defmodule ProteanIntegration.FinalStatesTest do
  use Protean.TestCase

  defmodule TestMachine do
    use Protean

    @machine [
      initial: "init",
      states: [
        init: [],
        simple_compound: [
          initial: "a",
          states: [
            a: [
              on: [b: "b"]
            ],
            b: [type: :final]
          ],
          done: "nearly_done"
        ],
        simple_parallel: [
          type: :parallel,
          states: [
            p1: [
              initial: "a",
              states: [
                a: [
                  on: [b: "b"]
                ],
                b: [type: :final]
              ]
            ],
            p2: [
              initial: "c",
              states: [
                c: [
                  on: [d: "d"]
                ],
                d: [type: :final]
              ]
            ]
          ],
          done: "nearly_done"
        ],
        nearly_done: [
          on: [
            [all_done: "all_done"]
          ]
        ],
        all_done: [
          type: :final
        ]
      ],
      on: [
        simple_compound: "#simple_compound",
        simple_parallel: "#simple_parallel"
      ]
    ]
  end

  @tag machine: TestMachine
  test "final states for compound nodes", %{machine: machine} do
    assert_protean(machine,
      send: "simple_compound",
      matches: "simple_compound.a",
      send: "b",
      matches: "nearly_done"
    )
  end

  @tag machine: TestMachine
  test "final states for parallel nodes", %{machine: machine} do
    assert_protean(machine,
      send: "simple_parallel",
      matches: "simple_parallel.p1.a",
      matches: "simple_parallel.p2.c",
      send: "b",
      matches: "simple_parallel.p1.b",
      send: "d",
      matches: "nearly_done"
    )
  end
end
