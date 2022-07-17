defmodule Protean.StateTest do
  use ExUnit.Case

  alias Protean.State

  test "State.matches? with explicit node ids" do
    [
      {[[]], [], true},
      {[["#"]], [], true},
      {[["#"]], ["#"], true},
      {[["foo", "#"]], ["#"], true},
      {[["#"]], ["foo", "#"], false},
      {[["foo", "bar", "#"]], ["bar", "#"], true},
      {[["foo", "bar", "#"]], ["foo", "baz", "#"], false},
      {[["foo", "#"], ["bar", "#"]], ["bar", "#"], true},
      {[["foo", "#"], ["baz", "bar", "#"]], ["bar", "#"], true}
    ]
    |> Enum.each(&matches_test/1)
  end

  test "State.matches? with shorthand" do
    [
      {[[]], "", false},
      {[["#"]], "", true},
      {[["#"]], "#", true},
      {[["foo", "#"]], "", true},
      {[["#"]], "foo", false},
      {[["foo", "#"]], "#foo", true},
      {[["foo", "#"]], "#.foo", true},
      {[["foo", "bar", "#"]], "bar", true},
      {[["foo", "bar", "#"]], "baz.foo", false},
      {[["foo", "#"], ["bar", "#"]], "bar", true},
      {[["foo", "#"], ["baz", "bar", "#"]], "bar", true},
      {[["foo", "#"], ["baz", "bar", "#"]], "bar.baz", true},
      {[["foo", "#"]], "#.##.##.foo", true}
    ]
    |> Enum.each(&matches_test/1)
  end

  defp matches_test({state_value, match_test, expected_result}) do
    with state <- %State{value: state_value} do
      assert State.matches?(state, match_test) === expected_result,
             "expected #{inspect(state_value)}" <>
               ((expected_result && " to match ") || " to NOT match ") <>
               "#{inspect(match_test)}"
    end
  end
end
