defmodule Protean.MacrosTest do
  use ExUnit.Case

  alias Protean.Machine

  defmodule MachineAsKeyword do
    use Protean,
      machine: [
        initial: :a,
        states: [a: []]
      ]
  end

  test "machines can be specified as a keyword to `use Protean`" do
    assert %Machine{} = MachineAsKeyword.machine()
  end

  defmodule MachineAsAttribute do
    use Protean

    @machine [
      initial: :a,
      states: [a: []]
    ]
  end

  test "machines can be specified in a @machine attribute" do
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

  defmodule InlineFunctionKeyword do
    use Protean,
      machine: [
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

  test "machines specified as a keyword allow inline functions" do
    assert %Machine{} = InlineFunctionKeyword.machine()
  end

  defmodule InlineFunctionAttribute do
    use Protean

    @machine [
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

  test "machines specified as an attribute allow inline functions" do
    assert %Machine{} = InlineFunctionAttribute.machine()
  end
end
