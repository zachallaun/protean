defmodule TestMachines do
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
end
