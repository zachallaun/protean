defmodule TestMachines do
  alias Protean.Action
  alias Protean.Interpreter
  alias Protean.MachineConfig

  def with_test_machine(%{machine: machine} = assigns) do
    assigns
    |> Map.merge(test_machine(machine))
  end

  def with_test_machine(%{machines: machines} = assigns) do
    assigns
    |> Map.put(:machines, Enum.map(machines, &test_machine/1))
  end

  def with_test_machine(assigns), do: assigns

  def test_machine(machine) do
    machine = apply(TestMachines, machine, [])

    case machine do
      %MachineConfig{} = config ->
        %{machine: config, initial: MachineConfig.initial_context(config)}

      module when is_atom(module) ->
        config = module.__protean_machine__()

        %{
          machine: config,
          initial: MachineConfig.initial_context(config),
          interpreter: Interpreter.new(machine: config)
        }
    end
  end

  def simple_machine_1 do
    Protean.MachineConfig.new(
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
    Protean.MachineConfig.new(
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
    Protean.MachineConfig.new(
      type: :parallel,
      states: [
        state_a: [],
        state_b: []
      ]
    )
  end

  def machine_with_actions_1 do
    Protean.MachineConfig.new(
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
    Protean.MachineConfig.new(
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
    use Protean

    @machine [
      assigns: %{
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
        #   [target: :straight, guard: "direction_straight?"],
        #   [target: :left, guard: "direction_left?"],
        #   [target: :right, guard: "direction_right?"]
        # ],
        go: [target: "#straight", guard: "direction_straight?"],
        go: [target: "#left", guard: "direction_left?"],
        go: [target: "#right", guard: "direction_right?"],
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
    def guard("direction_straight?", %{assigns: %{direction: :straight}}, _event), do: true
    def guard("direction_straight?", _, _), do: false

    def guard("direction_left?", %{assigns: %{direction: :left}}, _event), do: true
    def guard("direction_left?", _, _), do: false

    def guard("direction_right?", %{assigns: %{direction: :right}}, _event), do: true
    def guard("direction_right?", _, _), do: false
  end

  def silly_direction_machine do
    SillyDirectionMachine
  end

  defmodule PureMachine1 do
    use Protean

    @machine [
      initial: :a,
      assigns: %{
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
            a2: []
          ]
        ],
        b: []
      ]
    ]

    @impl true
    def handle_action("entering_a", %{assigns: %{acc: acc}} = context, _event) do
      Action.assign(context, :acc, ["entering_a" | acc])
    end

    def handle_action("exiting_a", %{assigns: %{acc: acc}} = context, _event) do
      Action.assign(context, :acc, ["exiting_a" | acc])
    end
  end

  def pure_machine_1 do
    PureMachine1
  end

  defmodule AutoTransitionMachine1 do
    use Protean

    @machine [
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
    AutoTransitionMachine1
  end

  defmodule AutoTransitionMachine2 do
    use Protean

    @machine [
      initial: :a,
      assigns: %{
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
    def handle_action(action_name, context, _event) do
      %{acc: acc} = context.assigns
      Action.assign(context, :acc, [action_name | acc])
    end
  end

  def auto_transition_machine_2 do
    AutoTransitionMachine2
  end
end
