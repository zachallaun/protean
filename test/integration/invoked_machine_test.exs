defmodule ProteanIntegration.InvokedMachineTest do
  use Protean.TestCase

  import ExUnit.CaptureLog

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
            {:grow_it, actions: [Action.send(:grow, to: "child")]},
            {match({:child_grown, _}), "relax"}
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
                Action.send({:child_grown, "I hath grown"}, to: :parent)
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
        call: :grow_it,
        sleep: 30,
        matches: "relax"
      )
    end
  end

  defmodule Crashes do
    use Protean

    @machine [
      initial: "can_crash",
      states: [
        can_crash: [
          on: [
            go_boom: [
              actions: ["crash"]
            ]
          ]
        ]
      ]
    ]

    @impl Protean
    def handle_action("crash", _, _) do
      raise "boom"
    end
  end

  defmodule InvokeCrashes do
    use Protean

    @machine [
      initial: "init",
      states: [
        init: [
          invoke: [
            id: "crashes",
            proc: ProteanIntegration.InvokedMachineTest.Crashes,
            error: [
              actions: ["save_event"],
              target: "invoke_crashed"
            ]
          ],
          on: [
            make_it_crash: [
              actions: [Protean.Action.send(:go_boom, to: "crashes")]
            ]
          ]
        ],
        invoke_crashed: []
      ]
    ]

    @impl Protean
    def handle_action("save_event", context, event) do
      Protean.Action.assign(context, :crash_event, event)
    end
  end

  describe "invoked machine crashes" do
    @describetag machine: InvokeCrashes

    test "trigger error transition", %{machine: machine} do
      error_message =
        capture_log(fn ->
          assert_protean(machine,
            call: :make_it_crash,
            sleep: 30,
            matches: "invoke_crashed"
          )
        end)

      assert error_message =~ "boom"
    end
  end

  defmodule ImmediatelyCrashes do
    use Protean

    @machine [
      initial: "crash_now",
      states: [
        crash_now: [
          always: [
            actions: ["crash"]
          ]
        ]
      ]
    ]

    @impl Protean
    def handle_action("crash", _, _) do
      raise "boom"
    end
  end

  defmodule InvokeImmediatelyCrashes do
    use Protean

    @machine [
      initial: "init",
      states: [
        init: [
          invoke: [
            id: "crashes",
            proc: ProteanIntegration.InvokedMachineTest.ImmediatelyCrashes,
            error: [
              target: "invoke_crashed"
            ]
          ]
        ],
        invoke_crashed: []
      ]
    ]
  end

  describe "invoked machine immediately crashes" do
    @describetag machine: InvokeImmediatelyCrashes

    test "trigger error transition", %{machine: machine} do
      capture_log(fn ->
        assert_protean(machine,
          sleep: 30,
          matches: "invoke_crashed"
        )
      end)
    end
  end
end
