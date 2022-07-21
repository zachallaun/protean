defmodule Protean.MachineConfigTest do
  use ExUnit.Case

  alias Protean.Action
  alias Protean.MachineConfig

  def delayed_transition_implicit do
    [
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
    ]
  end

  def delayed_transition_explicit do
    [
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
    ]
  end

  test "delayed transitions syntax" do
    [explicit, implicit] =
      [delayed_transition_explicit(), delayed_transition_implicit()]
      |> Enum.map(&MachineConfig.parse!/1)

    assert explicit == implicit
  end
end
