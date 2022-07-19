defmodule TestMachines do
  alias Protean.Action
  alias Protean.Interpreter
  alias Protean.Machine

  def with_test_machine(%{machine: machine} = context) do
    context
    |> Map.merge(test_machine(machine))
  end

  def with_test_machine(%{machines: machines} = context) do
    context
    |> Map.put(:machines, Enum.map(machines, &test_machine/1))
  end

  def with_test_machine(context), do: context

  def test_machine(machine) do
    machine = apply(TestMachines, machine, [])

    case machine do
      %Machine{} = machine ->
        %{machine: machine, initial: Machine.initial_state(machine)}

      {machine, handler} ->
        %{
          machine: machine,
          handler: handler,
          initial: Machine.initial_state(machine),
          interpreter: Interpreter.new(machine, handler)
        }
    end
  end

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

  def parallel_machine_with_actions_1 do
    Protean.Machine.new(
      initial: :parallel_state_a,
      states: [
        parallel_state_a: [
          type: :parallel,
          entry: ["entry_parallel_a"],
          exit: ["exit_parallel_a"],
          states: [
            state_a1: [
              entry: ["entry_a1"],
              exit: ["exit_a1"],
              on: [
                goto_b: [
                  actions: ["action_goto_b_1"],
                  target: :"#.state_b"
                ]
              ]
            ],
            state_a2: [
              entry: ["entry_a2"],
              exit: ["exit_a2"],
              initial: :foo,
              states: [
                foo: [
                  exit: ["exit_foo"],
                  on: [
                    foo_event: :bar
                  ]
                ],
                bar: [
                  entry: ["entry_bar"],
                  on: [
                    goto_b: [
                      actions: ["action_goto_b_2"],
                      target: :"#.state_b"
                    ]
                  ]
                ]
              ]
            ]
          ]
        ],
        state_b: [
          type: :final,
          entry: ["entry_b"]
        ]
      ]
    )
  end

  defmodule SillyDirectionMachine do
    use Protean,
      machine: [
        context: %{
          direction: :straight
        },
        initial: :straight,
        states: [
          straight: [],
          left: [],
          right: []
        ],
        on: [
          # go: [
          #   [target: :straight, when: "direction_straight?"],
          #   [target: :left, when: "direction_left?"],
          #   [target: :right, when: "direction_right?"]
          # ],
          go: [target: "#straight", when: "direction_straight?"],
          go: [target: "#left", when: "direction_left?"],
          go: [target: "#right", when: "direction_right?"],
          set_straight: [
            actions: [Action.assign(direction: :straight)]
          ],
          set_left: [
            actions: [Action.assign(direction: :left)]
          ],
          set_right: [
            actions: [Action.assign(direction: :right)]
          ]
        ]
      ]

    @impl true
    def condition("direction_straight?", _state, %{direction: :straight}, _event), do: true
    def condition("direction_straight?", _, _, _), do: false

    def condition("direction_left?", _state, %{direction: :left}, _event), do: true
    def condition("direction_left?", _, _, _), do: false

    def condition("direction_right?", _state, %{direction: :right}, _event), do: true
    def condition("direction_right?", _, _, _), do: false
  end

  def silly_direction_machine do
    {SillyDirectionMachine.protean_machine(), SillyDirectionMachine}
  end

  defmodule PureMachine1 do
    use Protean,
      machine: [
        initial: :a,
        context: %{
          acc: []
        },
        on: [
          goto_a: :a
        ],
        states: [
          a: [
            initial: :a1,
            entry: ["entering_a"],
            exit: ["exiting_a"],
            on: [
              goto_b: :b
            ],
            states: [
              a1: [
                on: [
                  goto_a2: :a2
                ]
              ],
              a2: [
                a2_goto_b: :"#b"
              ]
            ]
          ],
          b: []
        ]
      ]

    @impl true
    def pure("entering_a", state, %{acc: acc}) do
      Action.assign(state, :acc, ["entering_a" | acc])
    end

    def pure("exiting_a", state, %{acc: acc}) do
      Action.assign(state, :acc, ["exiting_a" | acc])
    end
  end

  def pure_machine_1 do
    {PureMachine1.protean_machine(), PureMachine1}
  end

  defmodule HigherOrderGuardMachine1 do
    use Protean,
      machine: [
        initial: :a,
        states: [
          a: [],
          b: [],
          c: [],
          d: []
        ],
        on: [
          goto_a: [
            target: "#a",
            when: {:not, {:in, "#d"}}
          ],
          goto_b: [
            target: "#b",
            when: {:in, "#a"}
          ],
          goto_c: [
            target: "#c",
            when: {:or, [{:in, "#d"}, {:in, "#b"}]}
          ],
          goto_d: [
            target: "#d",
            when: {:and, [{:in, "#c"}, "asked_nicely"]}
          ]
        ]
      ]

    @impl true
    def condition("asked_nicely", _state, _context, {_, :please}), do: true
  end

  def higher_order_guard_machine_1 do
    {HigherOrderGuardMachine1.protean_machine(), HigherOrderGuardMachine1}
  end

  defmodule HigherOrderGuardMachine2 do
    use Protean,
      machine: [
        initial: :a,
        states: [
          a: [],
          b: [],
          c: [],
          d: []
        ],
        on: [
          goto_a: [
            target: ".a",
            when: [:not, in: "d"]
          ],
          goto_b: [
            target: ".b",
            when: [in: "a"]
          ],
          goto_c: [
            target: ".c",
            when: [:or, in: "d", in: "b"]
          ],
          goto_d: [
            target: ".d",
            when: ["asked_nicely", in: "c"]
          ]
        ]
      ]

    @impl true
    def condition("asked_nicely", _state, _context, {_, :please}), do: true
  end

  def higher_order_guard_machine_2 do
    {HigherOrderGuardMachine2.protean_machine(), HigherOrderGuardMachine2}
  end

  defmodule AutoTransitionMachine1 do
    use Protean,
      machine: [
        initial: :a,
        states: [
          a: [
            always: :b
          ],
          b: []
        ]
      ]
  end

  def auto_transition_machine_1 do
    {AutoTransitionMachine1.protean_machine(), AutoTransitionMachine1}
  end

  defmodule AutoTransitionMachine2 do
    use Protean,
      machine: [
        initial: :a,
        context: %{
          acc: []
        },
        states: [
          a: [
            on: [
              goto_b: :b
            ]
          ],
          b: [
            always: [
              target: :c,
              actions: ["auto_to_c"]
            ]
          ],
          c: [
            always: [
              target: :d,
              actions: ["auto_to_d"]
            ]
          ],
          d: []
        ]
      ]

    @impl true
    def pure(action_name, state, %{acc: acc}) do
      Action.assign(state, :acc, [action_name | acc])
    end
  end

  def auto_transition_machine_2 do
    {AutoTransitionMachine2.protean_machine(), AutoTransitionMachine2}
  end

  def delayed_transition_machine_implicit do
    Machine.new(
      initial: :a,
      states: [
        a: [
          after: [
            delay: 2000,
            target: :b
          ],
          on: [
            goto_c: :c
          ]
        ],
        b: [
          after: [
            [
              delay: 1000,
              when: "some_condition",
              target: :c
            ],
            [
              delay: 2000,
              target: :c
            ]
          ]
        ],
        c: []
      ]
    )
  end

  def delayed_transition_machine_explicit do
    Machine.new(
      initial: :a,
      states: [
        a: [
          entry: [
            Action.send_event("$protean.after.2000-#a", delay: 2000)
          ],
          exit: [
            Action.cancel_event("$protean.after.2000-#a")
          ],
          on: [
            "$protean.after.2000-#a": [
              target: :b
            ],
            goto_c: :c
          ]
        ],
        b: [
          entry: [
            Action.send_event("$protean.after.1000-#b", delay: 1000),
            Action.send_event("$protean.after.2000-#b", delay: 2000)
          ],
          exit: [
            Action.cancel_event("$protean.after.1000-#b"),
            Action.cancel_event("$protean.after.2000-#b")
          ],
          on: [
            "$protean.after.1000-#b": [
              target: :c,
              when: "some_condition"
            ],
            "$protean.after.2000-#b": [
              target: :c
            ]
          ]
        ],
        c: []
      ]
    )
  end
end
