defmodule ProteanIntegration.HigherOrderGuardTest do
  use Protean.TestCase

  defmodule HigherOrderGuardMachine1 do
    use Protean

    @machine [
      initial: :a,
      states: [
        atomic(:a),
        atomic(:b),
        atomic(:c),
        atomic(:d)
      ],
      on: [
        {:goto_a, target: ".a", guard: {:not, {:in, "#d"}}},
        {:goto_b, target: ".b", guard: {:in, "#a"}},
        {:goto_c, target: ".c", guard: {:or, [{:in, "#d"}, {:in, "#b"}]}},
        match({:goto_d, _}, target: ".d", guard: {:and, [{:in, "#c"}, "asked_nicely"]})
      ]
    ]

    @impl true
    def guard("asked_nicely", _context, {_, :please}), do: true
  end

  defmodule HigherOrderGuardMachine2 do
    use Protean

    @machine [
      initial: :a,
      states: [
        atomic(:a),
        atomic(:b),
        atomic(:c),
        atomic(:d)
      ],
      on: [
        {:goto_a, target: ".a", guard: [:not, in: "d"]},
        {:goto_b, target: ".b", guard: [in: "a"]},
        {:goto_c, target: ".c", guard: [:or, in: "d", in: "b"]},
        match({:goto_d, _}, target: ".d", guard: ["asked_nicely", in: "c"])
      ]
    ]

    @impl true
    def guard("asked_nicely", _context, {_, :please}), do: true
  end

  @tag machine: HigherOrderGuardMachine1
  test "higher order guards", %{machine: machine} do
    higher_order_guard_test(machine)
  end

  @tag machine: HigherOrderGuardMachine2
  test "sugary higher order guards", %{machine: machine} do
    higher_order_guard_test(machine)
  end

  def higher_order_guard_test(machine) do
    assert_protean(machine,
      call: :goto_c,
      matches: "a",
      call: :goto_b,
      matches: "b",
      call: {:goto_d, nil},
      matches: "b",
      call: :goto_c,
      matches: "c",
      call: {:goto_d, nil},
      matches: "c",
      call: {:goto_d, :please},
      matches: "d",
      call: :goto_a,
      matches: "d",
      call: :goto_c,
      matches: "c",
      call: :goto_a,
      matches: "a"
    )
  end
end
