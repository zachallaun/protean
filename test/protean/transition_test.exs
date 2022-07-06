defmodule Protean.TransitionTest do
  use ExUnit.Case

  alias Protean.Transition

  test "event descriptor expansion" do
    cases = [
      foo: [["foo"]],
      "foo.bar": [["foo", "bar"]],
      "foo bar": [["foo"], ["bar"]],
      "foo.bar baz": [["foo", "bar"], ["baz"]],
      "foo.bar.": [["foo", "bar"]],
      "": [[]],
      .: [[]],
      *: [[]],
      "foo.": [["foo"]],
      "foo.*": [["foo"]],
      "foo*": [["foo*"]],
      "foo.* bar.*": [["foo"], ["bar"]]
    ]

    for {descriptor, expanded} <- cases do
      assert Transition.expand_event_descriptor(descriptor) == expanded,
             "expected #{inspect(descriptor)} to expand to #{inspect(expanded)}"
    end
  end

  test "expanded event descriptors expand to themselves" do
    descriptor = [["foo", "bar"]]
    assert Transition.expand_event_descriptor(descriptor) == descriptor
  end

  test "event descriptors match relevant events" do
    assert Transition.event_descriptor_match?(:foo, "foo") == true
    assert Transition.event_descriptor_match?(:foo, "fooo") == false
    assert Transition.event_descriptor_match?(:foo, "foo.bar") == true
    assert Transition.event_descriptor_match?(:"foo.bar", "foo") == false
    assert Transition.event_descriptor_match?(:"foo.*", "foo") == true
    assert Transition.event_descriptor_match?(:"foo.*", "fooo") == false
    assert Transition.event_descriptor_match?(:"foo.*", "foo.bar") == true
  end
end
