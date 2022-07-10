defmodule TestMachines do
  def with_test_machine(%{machine: machine} = context) do
    machine = apply(TestMachines, machine, [])

    context =
      context
      |> Map.put(:machine, machine)
      |> Map.put(:initial, Protean.Machine.initial_state(machine))

    {:ok, context}
  end

  def with_test_machine(context), do: context

  def simple_machine_1 do
    Protean.Machine.new(
      type: :compound,
      initial: :state_a,
      states: [
        state_a: [
          type: :atomic,
          on: [
            event_a: [
              target: :state_b
            ]
          ]
        ],
        state_b: [
          type: :final
        ]
      ]
    )
  end

  def simple_machine_2 do
    Protean.Machine.new(
      initial: :state_a,
      states: [
        state_a: [
          initial: :state_a1,
          states: [
            state_a1: [
              type: :compound,
              initial: :state_a1_child,
              states: [
                state_a1_child: []
              ],
              on: [
                event_a1: :state_b
              ]
            ]
          ],
          on: [
            event_a: [
              target: :state_b
            ]
          ]
        ],
        state_b: [
          type: :final
        ]
      ]
    )
  end

  def parallel_machine_1 do
    Protean.Machine.new(
      type: :parallel,
      states: [
        state_a: [],
        state_b: []
      ]
    )
  end

  def machine_with_actions_1 do
    Protean.Machine.new(
      initial: :state_a,
      states: [
        state_a: [
          entry: ["entry_a"],
          exit: ["exit_a"],
          on: [
            event_a: [
              actions: ["event_a_action"],
              target: :state_b
            ]
          ]
        ],
        state_b: [
          entry: ["entry_b"],
          exit: ["exit_b"],
          on: [
            event_b: [
              actions: ["event_b_action"],
              target: :state_c
            ]
          ]
        ],
        state_c: [
          type: :final,
          entry: ["entry_c"]
        ]
      ]
    )
  end
end
