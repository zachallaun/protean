defmodule Protean do
  @moduledoc """
  Protean is a library for modeling behaviour and managing side-effects with
  finite state machines and statecharts.

  ## The Protean Behaviour

  In order to use Protean, you need to:

    1. Define a module describing your machine configuration
    2. Define a module (possibly the same one) implementing any handlers
       specified in your machine configuration

  ### Example

  Protean is a process-based behaviour. You begin by defining a module that
  invokes `use Protean, machine: [ ... ]`. This will automatically define a
  default `start_link/1` and can be started by a supervisor.

      defmodule TrafficLight do
        use Protean, machine: [
          initial: :green,
          states: [
            green: [
              on: [NEXT: :yellow]
            ],
            yellow: [
              on: [NEXT: :red]
            ],
            red: [
              on: [NEXT: :green]
            ]
          ]
        ]
      end

  And elsewhere in your application:

      children = [
        # ...
        {TrafficLight, []}
      ]
      Supervisor.start_link(children, strategy: :one_for_one)

  The above will start the `TrafficLight` in its initial `:green` state, and we
  can interact with it in likely-obvious ways:

      %Protean.State{value: :green} =
        Protean.current(TrafficLight)

      # Send an event syncronously
      %Protean.State{value: :yellow} =
        Protean.send(TrafficLight, "NEXT")

      # Send an event asyncronously and await the result separately
      ref = Protean.send_async(TrafficLight, "NEXT")

      %Protean.State{value: :yellow} =
        Protean.await(TrafficLight, ref)

  Protean machines are built upon a purely functional core exposed through
  `Protean.Machine`.

      machine = Protean.Machine.new(TrafficLight)
      :green = machine.initial_state.value

      state = Protean.Machine.transition(machine, machine.initial_state, "NEXT")
      :yellow = state.value

      state = Protean.Machine.transition(machine, state, "NEXT")
      :red = state.value
  """

  @doc false
  defmacro __using__(opts) do
    config = Keyword.get(opts, :machine)

    quote do
      def protean_config() do
        unquote(config)
      end
    end
  end
end
