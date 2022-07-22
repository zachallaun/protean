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
      %Protean.State{value: MapSet<[["green", "#"]]>, ...}

      iex> Protean.send_event(TrafficLight, "NEXT")
      %Protean.State{value: MapSet<[["yellow", "#"]]>, ...}

      iex> Protean.send_event(TrafficLight, "NEXT")
      %Protean.State{value: MapSet<[["red", "#"]]>, ...}

      iex> Protean.send_event(TrafficLight, "UNKNOWN_EVENT")
      %Protean.State{value: MapSet<[["red", "#"]]>, ...}
  """

  alias Protean.Interpreter
  alias Protean.Interpreter.Server
  alias Protean.State

  defmacro __using__(opts) do
    unless __CALLER__.module do
      raise "`use Protean` outside of a module definition is not currently supported"
    end

    quote generated: true, location: :keep do
      use Protean.Macros, unquote(opts)
      :ok
    end
  end

  @doc "TODO"
  defdelegate send_event(pid, event), to: Server

  @doc "TODO"
  defdelegate send_event_async(pid, event), to: Server

  @doc "TODO"
  defdelegate send_event_after(pid, event, time), to: Server

  @doc "TODO"
  defdelegate current(pid), to: Server

  @doc "TODO"
  defdelegate stop(pid, reason), to: Server

  @doc "TODO"
  @spec matches?(State.t(), descriptor :: term) :: boolean
  @spec matches?(Interpreter.t(), descriptor :: term) :: boolean
  @spec matches?(GenServer.server(), descriptor :: term) :: boolean
  def matches?(item, descriptor)

  def matches?(%State{} = state, descriptor),
    do: State.matches?(state, descriptor)

  def matches?(%Interpreter{} = interpreter, descriptor) do
    interpreter
    |> Interpreter.state()
    |> State.matches?(descriptor)
  end

  def matches?(server, descriptor) do
    Server.matches?(server, descriptor)
  end
end
