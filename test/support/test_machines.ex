defmodule TestMachines do
  alias Protean.{Machine, Interpreter, Action}

  def with_test_machine(%{machine: machine} = context) do
    machine = apply(TestMachines, machine, [])

    context =
      case machine do
        %Machine{} = machine ->
          Map.merge(context, %{machine: machine, initial: Machine.initial_state(machine)})

        {machine, handler} ->
          Map.merge(context, %{
            machine: machine,
            handler: handler,
            initial: Machine.initial_state(machine),
            interpreter: Interpreter.new(machine, handler)
          })
      end

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

  def parallel_machine_with_actions_1 do
    Protean.Machine.new(
      initial: :parallel_state_a,
      states: [
        parallel_state_a: [
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
    def condition("direction_straight?", %{direction: :straight}, _, _), do: true
    def condition("direction_straight?", _, _, _), do: false

    def condition("direction_left?", %{direction: :left}, _, _), do: true
    def condition("direction_left?", _, _, _), do: false

    def condition("direction_right?", %{direction: :right}, _, _), do: true
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
    def pure("entering_a", context, _event, _meta) do
      %{context | acc: ["entering_a" | context.acc]}
    end

    def pure("exiting_a", context, _event, _meta) do
      %{context | acc: ["exiting_a" | context.acc]}
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
    def condition("asked_nicely", _ctx, {:event, _, :please}, _), do: true
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
    def condition("asked_nicely", _ctx, {:event, _, :please}, _), do: true
  end

  def higher_order_guard_machine_2 do
    {HigherOrderGuardMachine2.protean_machine(), HigherOrderGuardMachine2}
  end
end
