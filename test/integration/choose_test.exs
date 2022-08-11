defmodule ProteanIntegration.ChooseTest do
  use Protean.TestCase

  defmodule Choosey do
    use Protean
    alias Protean.Action

    @machine [
      assigns: [switch: "on", data: []],
      initial: "a",
      states: [
        a: [
          on: [
            {match({"set_switch", _}), actions: "set_switch"},
            {
              "make_a_choice",
              actions: [
                Action.choose([
                  {"set_data_a", guard: "switch_on"},
                  {"set_data_b", guard: "switch_off"},
                  {"set_data_d", guard: "switch_off"},
                  "set_data_c"
                ])
              ]
            },
            {
              "make_a_choice_pure",
              actions: "make_a_choice"
            }
          ]
        ]
      ]
    ]

    @impl Protean
    def handle_action("set_switch", context, {_, value}) do
      Action.assign(context, :switch, value)
    end

    def handle_action("set_data_" <> value, context, _event) do
      %{assigns: %{data: data}} = context
      Action.assign(context, :data, [value | data])
    end

    def handle_action("make_a_choice", context, _event) do
      Action.choose(context, [
        {"set_data_a", guard: "switch_on"},
        {"set_data_b", guard: "switch_off"},
        {"set_data_d", guard: "switch_off"},
        "set_data_c"
      ])
    end

    @impl Protean
    def guard("switch_" <> value, %{assigns: %{switch: value}}, _event), do: true
  end

  describe "choose action" do
    @describetag machine: Choosey

    test "inline chooses single action when its condition is true", %{machine: machine} do
      assert_protean(machine,
        call: "make_a_choice",
        assigns: [data: ["a"]],
        call: {"set_switch", "off"},
        call: "make_a_choice",
        assigns: [data: ["b", "a"]],
        call: {"set_switch", "indeterminate"},
        call: "make_a_choice",
        assigns: [data: ["c", "b", "a"]]
      )
    end

    test "pure chooses single action when its condition is true", %{machine: machine} do
      assert_protean(machine,
        call: "make_a_choice_pure",
        assigns: [data: ["a"]],
        call: {"set_switch", "off"},
        call: "make_a_choice_pure",
        assigns: [data: ["b", "a"]],
        call: {"set_switch", "indeterminate"},
        call: "make_a_choice_pure",
        assigns: [data: ["c", "b", "a"]]
      )
    end
  end
end
