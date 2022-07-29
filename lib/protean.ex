defmodule Protean do
  @external_resource "README.md"
  @moduledoc "README.md"
             |> File.read!()
             |> String.split("<!-- MDOC !-->")
             |> Enum.fetch!(1)

  alias Protean.Action
  alias Protean.Interpreter
  alias Protean.Interpreter.Server
  alias Protean.State

  @typedoc """
  A normalized event. Any events received from Protean will be of this form.
  """
  @type event :: {name :: String.t(), payload :: any}

  @typedoc """
  A sendable event. Events sent to Protean that match this type will be "desugared" to
  `t:event`.
  """
  @type sendable_event :: {String.t(), any} | {atom, any} | String.t() | atom

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
  @callback invoke(Action.name(), State.t(), event) :: any

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
  @callback pure(Action.name(), State.t(), event) :: State.t() | nil

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
  @callback effect(Action.name(), State.t(), event) :: any

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
  @callback condition(Action.name(), State.t(), event) :: boolean

  defmacro __using__(opts) do
    unless __CALLER__.module do
      raise "`use Protean` outside of a module definition is not currently supported"
    end

    quote generated: true, location: :keep do
      use Protean.Macros, unquote(opts)
      :ok
    end
  end

  @doc "Normalizes a `t:sendable_event` to a `t:event`."
  @spec event(sendable_event) :: event
  def event(name) when is_atom(name), do: {to_string(name), nil}
  def event(name) when is_binary(name), do: {name, nil}
  def event({name, payload}) when is_atom(name), do: {to_string(name), payload}
  def event({name, payload}) when is_binary(name), do: {name, payload}

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
