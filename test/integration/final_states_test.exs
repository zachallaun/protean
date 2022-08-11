defmodule ProteanIntegration.FinalStatesTest do
  use Protean.TestCase

  defmodule TestMachine do
    use Protean

    @machine [
      initial: :init,
      states: [
        atomic(:init),
        compound(:simple_compound,
          initial: "a",
          states: [
            atomic(:a, on: [b: "b"]),
            final(:b)
          ],
          done: "nearly_done"
        ),
        parallel(:simple_parallel,
          states: [
            compound(:p1,
              initial: "a",
              states: [
                atomic(:a, on: [b: "b"]),
                final(:b)
              ]
            ),
            compound(:p2,
              initial: "c",
              states: [
                atomic(:c, on: [d: "d"]),
                final(:d)
              ]
            )
          ],
          done: "nearly_done"
        ),
        atomic(:nearly_done,
          on: [
            all_done: "all_done"
          ]
        ),
        final(:all_done)
      ],
      on: [
        simple_compound: "#simple_compound",
        simple_parallel: "#simple_parallel"
      ]
    ]
  end

  describe "final state" do
    @describetag machine: TestMachine

    test "in compound nodes", %{machine: machine} do
      assert_protean(machine,
        call: :simple_compound,
        matches: "simple_compound.a",
        call: :b,
        matches: "nearly_done"
      )
    end

    test "in parallel nodes", %{machine: machine} do
      assert_protean(machine,
        call: :simple_parallel,
        matches: "simple_parallel.p1.a",
        matches: "simple_parallel.p2.c",
        call: :b,
        matches: "simple_parallel.p1.b",
        call: :d,
        matches: "nearly_done"
      )
    end

    test "at root", %{machine: machine} do
      assert_protean(machine,
        call: :simple_compound,
        call: :b,
        matches: "nearly_done",
        call: :all_done
      )

      assert not Process.alive?(machine)
      assert_received {:EXIT, _pid, {:shutdown, %Protean.Context{}}}
    end
  end
end
