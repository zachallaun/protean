defmodule ProteanIntegration.InvokedMachineTest do
  use Protean.TestCase

  import ExUnit.CaptureLog

  defmodule Parent do
    use Protean
    alias Protean.Action

    defmachine(
      initial: "parenting",
      states: [
        parenting: [
          invoke: [
            id: "child",
            proc: ProteanIntegration.InvokedMachineTest.Child
          ],
          on: [
            {:grow_it, actions: [Action.send_event(:grow, to: "child")]},
            {{:child_grown, _}, "relax"}
          ]
        ],
        relax: []
      ]
    )
  end

  defmodule Child do
    use Protean
    alias Protean.Action

    defmachine(
      initial: "growing",
      states: [
        growing: [
          on: [
            grow: [
              actions: [
                Action.send_event({:child_grown, "I hath grown"}, to: :parent)
              ],
              target: "grown"
            ]
          ]
        ],
        grown: []
      ]
    )
  end

  describe "the parent/child relationship" do
    @describetag machine: Parent

    test "sending events between parent/child", %{machine: parent} do
      assert_protean(parent,
        send: :grow_it,
        sleep: 30,
        matches: "relax"
      )
    end
  end

  defmodule Crashes do
    use Protean

    defmachine(
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
    )

    @impl Protean
    def action("crash", _, _) do
      raise "boom"
    end
  end

  defmodule InvokeCrashes do
    use Protean

    defmachine(
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
              actions: [Protean.Action.send_event(:go_boom, to: "crashes")]
            ]
          ]
        ],
        invoke_crashed: []
      ]
    )

    @impl Protean
    def action("save_event", state, event) do
      Protean.Action.assign(state, :crash_event, event)
    end
  end

  describe "invoked machine crashes" do
    @describetag machine: InvokeCrashes

    test "trigger error transition", %{machine: machine} do
      error_message =
        capture_log(fn ->
          assert_protean(machine,
            send: :make_it_crash,
            sleep: 30,
            matches: "invoke_crashed"
          )
        end)

      assert error_message =~ "boom"
    end
  end

  defmodule ImmediatelyCrashes do
    use Protean

    defmachine(
      initial: "crash_now",
      states: [
        crash_now: [
          always: [
            actions: ["crash"]
          ]
        ]
      ]
    )

    @impl Protean
    def action("crash", _, _) do
      raise "boom"
    end
  end

  defmodule InvokeImmediatelyCrashes do
    use Protean

    defmachine(
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
    )
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
