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

  alias Protean.Action
  alias Protean.Interpreter
  alias Protean.Interpreter.Server
  alias Protean.Machine
  alias Protean.State

  @doc """
  Used to define invoked services at runtime. Returns a value or child spec usable by the invoke
  type.

  ### Example

      @machine [
        # ...
        states: [
          # ...
          awaiting_task: [
            invoke: [
              task: "my_task",
              done: "task_complete"
            ]
          ],
          task_complete: [
            # ...
          ]
        ]
      ]

      @impl Protean
      def invoke("my_task", _state, {"trigger", data}) do
        {__MODULE__, :run_task, [data]}
      end
  """
  @callback invoke(Action.name(), State.t(), Machine.event()) :: any

  @doc """
  Used to modify machine context and trigger additional actions. Returns the machine state.

  This callback should not produce any side-effects, but should instead leave side-effects to
  `c:effect/3` or return actions describing side-effects that will be executed by the
  interpreter. This ensures that all effects take place in the proper order or can be
  appropriately canceled as a result of later actions.

  ### Example

      @machine [
        # ...
        states: [
          # ...
          waiting: [
            on: [
              data_event: [
                target: "data_received",
                actions: ["assign_and_send_data"]
              ]
            ]
          ]
        ]
      ]

      @impl Protean
      def pure("assign_and_send_data", state, {"data_event", data}) do
        %{service: pid} = state.context

        state
        |> Protean.Action.send_event({"data_received", data}, to: pid)
        |> Protean.Action.assign(:last_received_data, data)
      end
  """
  @callback pure(Action.name(), State.t(), Machine.event()) :: State.t() | nil

  @doc """
  Used to perform arbitrary side-effects.

  ### Example

      @machine [
        # ...
        states: [
          # ...
          editing_user: [
            on: [
              "user.commit": [
                actions: ["broadcast"],
                target: "viewing_user"
              ]
            ]
          ],
          viewing_user: [
            # ...
          ]
        ]
      ]

      @impl Protean
      def effect("broadcast", state, {"user.commit", user}) do
        %{topic: topic} = state.context

        PubSub.broadcast(topic, "user:\#{user.id}", {:user_update, user})
      end
  """
  @callback effect(Action.name(), State.t(), Machine.event()) :: any

  @doc """
  Used to determine whether a transition should take place.

  ### Example

      @machine [
        # ...
        states: [
          editing_user: [
            on: [
              "user.commit": [
                when: "user_valid?",
                actions: ["broadcast"],
                target: "viewing_user"
              ],
              "user.commit": [
                when: {:not, "user_valid?"},
                actions: ["show_invalid_user_error"]
              ]
            ]
          ]
        ]
      ]

      @impl Protean
      def condition("user_valid?", state, {_, user}) do
        User.changeset(%User{}, user).valid?
      end
  """
  @callback condition(Action.name(), State.t(), Machine.event()) :: boolean

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
