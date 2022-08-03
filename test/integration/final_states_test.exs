defmodule ProteanIntegration.FinalStatesTest do
  use Protean.TestCase

  defmodule TestMachine do
    use Protean

    defmachine(
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
            all_done: "all_done"
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
    )
  end

  describe "final state" do
    @describetag machine: TestMachine

    test "in compound nodes", %{machine: machine} do
      assert_protean(machine,
        send: :simple_compound,
        matches: "simple_compound.a",
        send: :b,
        matches: "nearly_done"
      )
    end

    test "in parallel nodes", %{machine: machine} do
      assert_protean(machine,
        send: :simple_parallel,
        matches: "simple_parallel.p1.a",
        matches: "simple_parallel.p2.c",
        send: :b,
        matches: "simple_parallel.p1.b",
        send: :d,
        matches: "nearly_done"
      )
    end

    test "at root", %{machine: machine} do
      assert_protean(machine,
        send: :simple_compound,
        send: :b,
        matches: "nearly_done",
        send: :all_done
      )

      assert not Process.alive?(machine)
      assert_received {:EXIT, _pid, {:shutdown, %Protean.State{}}}
    end
  end
end
