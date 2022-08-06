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
            matches: "a",
            send: "goto_b",
            matches: "b",
            context: [foo: 1]
          ])
        end
      end

  ### Tags

  Provides the following tag usage through ExUnit's `setup` and `on_exit` callbacks:

    * `@tag machine: Machine` - Starts and monitors the machine process. Adds the following to
       context: `%{machine: machine_pid, ref: monitor_ref}`
    * `@tag machine: {Machine, opts}` - Like the above but calls `Machine.start_link(opts)` to
      start the process with the given options.
    * `@tag machines: [{Machine, opts}, Machine]` - Will start all machines in the list with
      given options and will add to context: `%{machines: [%{machine: pid, ref: ref}, ...]}`

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
    Process.flag(:trap_exit, true)
    setup_context(context)
  end

  defp setup_context(%{machines: machines}) when is_list(machines) do
    {all_machines, exit_funs} =
      machines
      |> Enum.map(&setup_machine/1)
      |> Enum.unzip()

    on_exit(fn -> Enum.each(exit_funs, & &1.()) end)

    [machines: all_machines]
  end

  defp setup_context(%{machine: machine}) do
    {context_to_add, exit_fun} = setup_machine(machine)

    on_exit(exit_fun)

    context_to_add
  end

  defp setup_context(_other), do: :ok

  @doc """
  Runs through a list of instructions in order, sending events to and making assertions on the
  running machine process.
  """
  def assert_protean(pid, instructions) do
    Enum.reduce(instructions, nil, fn
      # Actions
      {:call, event}, _ ->
        Protean.call(pid, event)

      {:send_async, event}, _ ->
        Protean.send(pid, event)
        nil

      # Utilities
      {:sleep, milliseconds}, _ when is_integer(milliseconds) ->
        :timer.sleep(milliseconds)
        nil

      # Assertions
      {:matches, descriptor}, acc ->
        state = Protean.current(pid)
        assert Protean.matches?(state, descriptor)
        acc

      {:context, context}, acc ->
        current_context = Protean.current(pid).context

        for {key, value} <- Enum.into(context, []) do
          assert current_context[key] == value
        end

        acc

      {:last_matches, descriptor}, {state, _} = acc ->
        assert Protean.matches?(state, descriptor)
        acc

      {:last_context, context}, {state, _} = acc ->
        for {key, value} <- Enum.into(context, []) do
          assert state.context[key] == value
        end

        acc

      {:last_replies, expected}, {_, actual} = acc ->
        assert actual == expected
        acc

      {:last_matches, _}, nil ->
        raise "Used :last_matches but have no stored result"

      {:last_context, _}, nil ->
        raise "Used :last_context but have no stored result"

      {:last_replies, _}, nil ->
        raise "Used :last_replies but have no stored result"

      other, _ ->
        raise "Unknown `assert_protean/2` instruction: #{inspect(other)}"
    end)
  end

  defp setup_machine({module, opts}) do
    {:ok, pid} = module.start_link(opts)
    ref = Process.monitor(pid)
    :ok = Protean.ping(pid)

    {%{machine: pid, ref: ref}, fn -> Process.exit(pid, :normal) end}
  end

  defp setup_machine(module), do: setup_machine({module, []})
end
