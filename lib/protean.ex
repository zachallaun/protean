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
  @type event :: {name :: String.t(), payload :: term()}

  @typedoc """
  A sendable event. Events sent to Protean that match this type will be "desugared" to
  `t:event`.
  """
  @type sendable_event :: {String.t(), term()} | {atom(), term()} | String.t() | atom()

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
  @callback invoke(Action.name(), State.t(), event) :: term()

  @doc """
  Used to execute actions in response to machine transitions.

  Receives the current machine state and event triggering the action as arguments and must return
  the machine state. It is possible to attach actions to the machine state to indicate that they
  should be performed immediately following this action. See `Protean.Action`.

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
      def action("assign_and_send_data", state, {"data_event", data}) do
        %{service: pid} = state.context

        PubSub.broadcast!(@pubsub, @topic, data)

        state
        |> Protean.Action.send_event({"data_received", data}, to: pid)
        |> Protean.Action.assign(:last_received_data, data)
      end
  """
  @callback action(Action.name(), State.t(), event) :: State.t()

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
  @callback condition(Action.name(), State.t(), event) :: boolean()

  defmacro __using__(opts) do
    unless __CALLER__.module do
      raise "`use Protean` outside of a module definition is not currently supported"
    end

    quote generated: true, location: :keep do
      use Protean.Macros, unquote(opts)
      :ok
    end
  end

  @doc """
  Starts an instance of the Protean supervisor, which allows Protean to manage processes invoked
  by a machine. For options, see `Supervisor.start_link/2`. If not specified, the supervisor name
  will default to `Protean`.
  """
  def start_link(opts \\ []) do
    DynamicSupervisor.start_link(
      Protean.DynamicSupervisor,
      [],
      Keyword.merge([name: Protean], opts)
    )
  end

  def child_spec(opts) do
    %{
      id: Keyword.get(opts, :id, Protean),
      start: {Protean, :start_link, [opts]},
      type: :supervisor
    }
  end

  @doc "Normalizes a `t:sendable_event` to a `t:event`."
  @spec event(sendable_event) :: event
  def event(name) when is_atom(name), do: {to_string(name), nil}
  def event(name) when is_binary(name), do: {name, nil}
  def event({name, payload}) when is_atom(name), do: {to_string(name), payload}
  def event({name, payload}) when is_binary(name), do: {name, payload}

  @doc "TODO"
  defdelegate send_event(protean, event), to: Server

  @doc "TODO"
  defdelegate send_event_async(protean, event), to: Server

  @doc "TODO"
  defdelegate send_event_after(protean, event, time), to: Server

  @doc "TODO"
  defdelegate current(protean), to: Server

  @doc "TODO"
  defdelegate stop(protean, reason), to: Server
  defdelegate stop(protean), to: Server

  @doc """
  Subscribes the caller to the running Protean machine, returning a reference.

  Processes subscribed to a machine will receive messages whenever the machine transitions. (Note
  that a machine can transition to the same state it was in previously.) By default, subscribed
  processes also monitor the machine (see `Process.monitor/1`). This behavior can be changed by
  passing `monitor: false`.

  Messages on transition will be delivered in the shape of:

      {:state, state, ref}

  where:

    * `state` is the `Protean.State` resulting from the transition;
    * `ref` is a monitor reference.
  """
  @spec subscribe(Server.server(), keyword()) :: reference()
  defdelegate subscribe(protean, opts \\ [monitor: true]), to: Server

  @doc "Unsubscribes the caller from the running Protean machine."
  defdelegate unsubscribe(protean, ref), to: Server

  @doc false
  defdelegate ping(pid), to: Server

  @doc "TODO"
  @spec matches?(State.t(), descriptor :: term()) :: boolean()
  @spec matches?(Interpreter.t(), descriptor :: term()) :: boolean()
  @spec matches?(Server.server(), descriptor :: term()) :: boolean()
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
