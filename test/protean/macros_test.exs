defmodule Protean.MacrosTest do
  use ExUnit.Case

  alias Protean.Machine

  defmodule MachineAsAttribute do
    use Protean

    defmachine [
      initial: :a,
      states: [a: []]
    ]
  end

  test "machines can be specified with defmachine" do
    assert %Machine{} = MachineAsAttribute.machine()
  end

  defmodule MachineAsFunction do
    use Protean

    def machine do
      Protean.Machine.new(
        initial: :a,
        states: [a: []]
      )
    end
  end

  test "machines can be specified in a function" do
    assert %Machine{} = MachineAsAttribute.machine()
  end

  defmodule InlineFunctionAttribute do
    use Protean

    defmachine [
      context: %{},
      initial: :a,
      states: [
        a: [
          entry: [
            Protean.Action.assign(fn _, _, _ -> %{} end)
          ]
        ]
      ]
    ]
  end

  test "machines specified with defmachine allow inline functions" do
    assert %Machine{} = InlineFunctionAttribute.machine()
  end
end
