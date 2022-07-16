defmodule Protean do
  @moduledoc """
  Protean is a library for modeling state and managing side-effects with
  finite state machines and statecharts.

  ## The Protean Behaviour

  Protean is a process-based behaviour built on top of `GenServer`. To use
  Protean, you need to:

    1. Define a module that invokes `use Protean`
    2. Specify your state machine configuration
    3. Implement handlers for dynamic or side-effecting behavior specified in
       your state machine

  ### Basic example

  Here's how you could define an extremely simple traffic light that simply
  cycles between green, yellow, and red:

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

  This module now defines a default `child_spec/1` and can be started under
  a supervisor.

      Supervisor.start_link([TrafficLight], strategy: :one_for_one)

  If multiple machines will be started, an `:id` can be passed as well,
  supporting all of the options available when starting a `GenServer`.

      children = [
        {TrafficLight, [gen_server: [name: TrafficLight1]]},
        {TrafficLight, [gen_server: [name: TrafficLight2]]}
      ]
      Supervisor.start_link(children, strategy: :one_for_one)

  The above will start the `TrafficLight` in its initial state. We can get the
  current state and send events to transition the state:

      iex> Protean.current(TrafficLight)
      %Protean.State{value: [["green", "#"]], ...}

      iex> Protean.send(TrafficLight, "NEXT")
      %Protean.State{value: [["yellow", "#"]], ...}

      iex> Protean.send(TrafficLight, "NEXT")
      %Protean.State{value: [["red", "#"]], ...}

      iex> Protean.send(TrafficLight, "UNKNOWN_EVENT")
      %Protean.State{value: [["red", "#"]], ...}
  """

  alias Protean.Interpreter.Server

  @doc false
  defmacro __using__(opts) do
    config = Keyword.fetch!(opts, :machine)

    quote location: :keep, bind_quoted: [config: config] do
      @behaviour Protean.Action.Pure
      @behaviour Protean.Action.Effect

      unless Module.has_attribute?(__MODULE__, :doc) do
        @doc """
        Returns a specification to start a `Protean.Interpreter` under a
        supervisor using the machine configuration defined in this module.
        """
      end

      def child_spec(opts) do
        {id, opts} = Keyword.pop(opts, :id, __MODULE__)

        defaults = [
          handler: __MODULE__,
          machine: protean_machine(),
          gen_server: [name: __MODULE__]
        ]

        spec = %{
          id: id,
          start: {Protean.Interpreter.Server, :start_link, [Keyword.merge(defaults, opts)]}
        }

        Supervisor.child_spec(spec, [])
      end

      defoverridable child_spec: 1

      def protean_machine do
        Protean.Machine.new(unquote(Macro.escape(config)))
      end

      @before_compile Protean
    end
  end

  @doc false
  defmacro __before_compile__(_env) do
    quote do
      @impl Protean.Action.Pure
      def pure(_, _, _, _), do: nil

      @impl Protean.Action.Effect
      def effect(_, _, _, _), do: nil
    end
  end

  defdelegate send(pid, event), to: Server
  defdelegate send_async(pid, event), to: Server
  defdelegate current(pid), to: Server
  defdelegate stop(pid), to: Server
end
