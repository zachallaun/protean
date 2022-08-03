defmodule Protean do
  @external_resource "README.md"
  @moduledoc "README.md"
             |> File.read!()
             |> String.split("<!-- MDOC !-->")
             |> Enum.fetch!(1)

  import Kernel, except: [send: 2]

  alias Protean.Action
  alias Protean.Interpreter
  alias Protean.Interpreter.Server
  alias Protean.State

  @typedoc "A running Protean machine process."
  @type server :: GenServer.server()

  @typedoc "Any message sent to a Protean machine."
  @type event :: term()

  @typedoc "Options for `subscribe/2`."
  @type subscribe_option :: {:monitor, boolean()}

  @doc """
  Used to define invoked services at runtime. Returns a value or child spec usable by the invoke
  type.

  ### Example

      defmachine(
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
      )

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

      defmachine [
        # ...
        states: [
          # ...
          state: [
            on: [
              {{:data, _any}, target: "data_received", actions: "assign_and_send_data"}
            ]
          ]
        ]
      ]

      @impl Protean
      def action("assign_and_send_data", state, {:data, data}) do
        %{service: pid} = state.context

        PubSub.broadcast!(@pubsub, @topic, data)

        state
        |> Protean.Action.send({"data_received", data}, to: pid)
        |> Protean.Action.assign(:last_received_data, data)
      end
  """
  @callback action(Action.name(), State.t(), event) :: State.t()

  @doc """
  Used to determine whether a transition should take place.

  ### Example

      defmachine [
        # ...
        states: [
          editing_user: [
            on: [
              {
                {:user_commit, _},
                when: "user_valid?",
                actions: ["broadcast"],
                target: "viewing_user"
              },
              {
                {:user_commit, _},
                when: {:not, "user_valid?"},
                actions: ["show_invalid_user_error"]
              }
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
  Makes a synchronous call to the machine and waits for it to execute any transitions
  that result from the given event.

  Shares semantics with `GenServer.call/3`.
  """
  @spec call(server, event) :: State.t()
  @spec call(server, event, timeout()) :: State.t()
  defdelegate call(protean, event), to: Server
  defdelegate call(protean, event, timeout), to: Server

  @doc """
  Sends an asynchronous event to the machine.

  Shares semantics with `GenServer.cast/2`.
  """
  @spec send(server, event) :: :ok
  defdelegate send(protean, event), to: Server

  @doc """
  Sends an event to the machine after `time` in milliseconds has passed.

  Returns a timer reference that can be canceled with `Process.cancel_timer/1`.
  """
  @spec send_after(server, event, non_neg_integer()) :: reference()
  defdelegate send_after(protean, event, time), to: Server

  @doc """
  Synchronously retrieve the current machine state.

  TODO: Allow optional timeout as with `call/3`.
  """
  @spec current(server) :: State.t()
  defdelegate current(protean), to: Server

  @doc "TODO"
  defdelegate stop(protean, reason), to: Server
  defdelegate stop(protean), to: Server

  @doc """
  Subscribes the caller to a running machine, returning a reference.

  Processes subscribed to a machine will receive messages whenever the machine transitions. (Note
  that a machine can transition to the same state it was in previously.) By default, subscribed
  processes also monitor the machine (see `Process.monitor/1`). This behavior can be changed by
  passing `monitor: false`.

  Messages on transition will be delivered in the shape of:

      {:state, state, ref}

  where:

    * `state` is the `Protean.State` resulting from the transition;
    * `ref` is a monitor reference.

  As with monitor, if the process is already dead when calling `Protean.subscribe/2`, a `:DOWN`
  message is delivered immediately.
  """
  @spec subscribe(server, [subscribe_option]) :: reference()
  defdelegate subscribe(protean, opts \\ [monitor: true]), to: Server

  @doc "Unsubscribes the caller from the machine."
  @spec unsubscribe(server, reference()) :: :ok
  defdelegate unsubscribe(protean, ref), to: Server

  @doc false
  defdelegate ping(pid), to: Server

  @doc """
  Returns true if the given machine state is in the given state.

  Note that calling `matches?/2` on a machine process is a synchronous operation that is
  equivalent to:

      machine |> Protean.current() |> Protean.matches?(descriptor)

  """
  @spec matches?(State.t(), descriptor :: term()) :: boolean()
  @spec matches?(server, descriptor :: term()) :: boolean()
  def matches?(item, descriptor)

  def matches?(%State{} = state, descriptor) do
    State.matches?(state, descriptor)
  end

  def matches?(%Interpreter{} = interpreter, descriptor) do
    interpreter
    |> Interpreter.state()
    |> State.matches?(descriptor)
  end

  def matches?(protean, descriptor) do
    Server.matches?(protean, descriptor)
  end
end
