defmodule Protean.TestCase do
  @moduledoc """
  Utilities for testing modules defined with `use Protean`.

  ## Usage

      defmodule ProteanIntegration.ExampleTest do
        use Protean.TestCase

        defmodule ExampleMachine do
          use Protean

          @machine [
            context: [
              foo: 1,
              bar: 2
            ],
            initial: :a,
            states: [
              a: [
                on: [goto_b: :b]
              ],
              b: []
            ]
          ]
        end

        @moduletag machine: ExampleMachine

        test "example", %{machine: machine} do
          assert_protean(machine, [
            {:matches?, :a},
            {:send, "goto_b"},
            {:matches?, :b},
            {:context, foo: 1}
          ])
        end
      end

  ### Tags

  Provides the following tag usage through ExUnit's `setup` and `on_exit` callbacks:

    * `@tag machine: Machine` - Starts and monitors the machine process. Adds the following to
       context: `%{machine: machine_pid, ref: monitor_ref}`
    * `@tag machine: {Machine, opts}` - Like the above but calls `Machine.start_link(opts)` to
      start the process with the given options.

  ### Helpers

  Provides test helper to turn a list of instructions into events sent to the machine and
  assertions on the machine's state. See `assert_protean/2`.
  """

  use ExUnit.CaseTemplate
  import ExUnit.Assertions

  using do
    quote do
      import Protean.TestCase
    end
  end

  setup context do
    if machine = context[:machine] do
      setup_machine(machine)
    end
  end

  @doc """
  Runs through a list of instructions in order, sending events to and making assertions on the
  running machine process.
  """
  def assert_protean(pid, instructions) do
    Enum.each(instructions, fn
      # Actions
      {:send, event} ->
        Protean.send(pid, event)

      {:send, event, data} ->
        Protean.send(pid, {event, data})

      # Utilities
      {:sleep, milliseconds} when is_integer(milliseconds) ->
        :timer.sleep(milliseconds)

      # Assertions
      {:matches?, descriptor} ->
        assert Protean.matches?(pid, descriptor)

      {:context, context} ->
        current_context = Protean.current(pid).context

        for {key, value} <- Enum.into(context, []) do
          assert current_context[key] == value
        end

      other ->
        raise "Unknown `assert_protean/2` instruction: #{inspect(other)}"
    end)
  end

  defp setup_machine({module, opts}) do
    {:ok, pid} = module.start_link(opts)
    ref = Process.monitor(pid)

    on_exit(fn -> Process.exit(pid, :normal) end)

    [machine: pid, ref: ref]
  end

  defp setup_machine(module), do: setup_machine({module, []})
end
