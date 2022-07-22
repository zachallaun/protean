defmodule ProteanIntegration.InvokedMachineTest do
  use Protean.TestCase, async: true

  defmodule Parent do
    use Protean
    alias Protean.Action

    @machine [
      initial: "parenting",
      states: [
        parenting: [
          invoke: [
            id: "child",
            proc: ProteanIntegration.InvokedMachineTest.Child
          ],
          on: [
            grow_it: [
              actions: [
                Action.send_event("grow", to: "child")
              ]
            ],
            child_grown: "relax"
          ]
        ],
        relax: []
      ]
    ]
  end

  defmodule Child do
    use Protean
    alias Protean.Action

    @machine [
      initial: "growing",
      states: [
        growing: [
          on: [
            grow: [
              actions: [
                Action.send_event({"child_grown", "I hath grown"}, to: :parent)
              ],
              target: "grown"
            ]
          ]
        ],
        grown: []
      ]
    ]
  end

  describe "the parent/child relationship" do
    @describetag machine: Parent

    test "sending events between parent/child", %{machine: parent} do
      assert_protean(parent,
        send: "grow_it",
        sleep: 30,
        matches: "relax"
      )
    end
  end
end
