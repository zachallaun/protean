defmodule ProteanIntegration.ChooseTest do
  use Protean.TestCase

  defmodule Choosey do
    use Protean
    alias Protean.Action

    defmachine(
      context: [switch: "on", data: []],
      initial: "a",
      states: [
        a: [
          on: [
            {{"set_switch", _}, actions: "set_switch"},
            {
              "make_a_choice",
              actions: [
                Action.choose([
                  {"set_data_a", when: "switch_on"},
                  {"set_data_b", when: "switch_off"},
                  {"set_data_d", when: "switch_off"},
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
    )

    @impl Protean
    def action("set_switch", state, {_, value}) do
      Action.assign(state, :switch, value)
    end

    def action("set_data_" <> value, state, _event) do
      %{context: %{data: data}} = state
      Action.assign(state, :data, [value | data])
    end

    def action("make_a_choice", state, _event) do
      Action.choose(state, [
        {"set_data_a", when: "switch_on"},
        {"set_data_b", when: "switch_off"},
        {"set_data_d", when: "switch_off"},
        "set_data_c"
      ])
    end

    @impl Protean
    def condition("switch_" <> value, %{context: %{switch: value}}, _event), do: true
  end

  describe "choose action" do
    @describetag machine: Choosey

    test "inline chooses single action when its condition is true", %{machine: machine} do
      assert_protean(machine,
        call: "make_a_choice",
        context: [data: ["a"]],
        call: {"set_switch", "off"},
        call: "make_a_choice",
        context: [data: ["b", "a"]],
        call: {"set_switch", "indeterminate"},
        call: "make_a_choice",
        context: [data: ["c", "b", "a"]]
      )
    end

    test "pure chooses single action when its condition is true", %{machine: machine} do
      assert_protean(machine,
        call: "make_a_choice_pure",
        context: [data: ["a"]],
        call: {"set_switch", "off"},
        call: "make_a_choice_pure",
        context: [data: ["b", "a"]],
        call: {"set_switch", "indeterminate"},
        call: "make_a_choice_pure",
        context: [data: ["c", "b", "a"]]
      )
    end
  end
end
