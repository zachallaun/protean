defmodule Protean do
  @external_resource "README.md"
  @moduledoc "README.md"
             |> File.read!()
             |> String.split("<!-- MDOC !-->")
             |> Enum.fetch!(1)

  import Kernel, except: [send: 2]

  alias Protean.Interpreter
  alias Protean.Interpreter.Server
  alias Protean.MachineConfig
  alias Protean.State

  @typedoc "A running Protean machine process."
  @type server :: GenServer.server()

  @typedoc "Any message sent to a Protean machine."
  @type event :: term()

  @typedoc "Option values for `start*` functions."
  @type start_option :: machine_option | GenServer.option()

  @typedoc "Option values for Protean machines."
  @type machine_option ::
          {:context, State.context()}
          | {:supervisor, Supervisor.name()}
          | {:machine, MachineConfig.t()}
          | {:module, module()}
          | {:parent, server | pid()}

  @typedoc "Option values for `subscribe/2`."
  @type subscribe_option ::
          {:monitor, boolean()}
          | {:to, subscribe_to_option}

  @type subscribe_to_option :: :all | :answer

  @typedoc "Option values for `use Protean`."
  @type using_option :: {:callback_module, module()}

  @protean_options [:machine, :callback_module]
  @protean_options_attr :"$protean.options"
  @protean_machine_attr :"$protean.machine"

  @doc """
  Optional callback for invoked processes specified during machine execution.

  Should return a value or child specification for the type of process being invoked.

  ## Example

      @machine [
        # ...
        states: [
          # ...
          awaiting_task: [
            invoke: [
              task: "my_task",
              done: "completed"
            ]
          ],
          completed: [
            # ...
          ]
        ]
      ]

      @impl true
      def invoke("my_task", _state, event_data) do
        {__MODULE__, :run_my_task, [event_data]}
      end

  """
  @callback invoke(term(), State.t(), event) :: term()

  @doc """
  Optional callback for actions specified in response to a transition.

  Receives the current machine state and event triggering the action as arguments. Returns one
  of:

    * `state` - same as `{:noreply, state}`
    * `{:noreply, state}` - the machine state with any new actions
    * `{:reply, reply, state}` - a reply and the machine state with any new actions

  ## Example

      @machine [
        # ...
        on: [
          {
            match({:data, _any}),
            target: :data_received,
            actions: [:assign_data, :broadcast_data]
          }
        ]
      ]

      @impl true
      def handle_action(:assign_data, state, {:data, data}) do
        state
        |> Protean.Action.assign(:last_received, data)
      end

      def handle_action(:broadcast_data, state, _) do
        %{notify: pid, last_received: data} = state.context

        PubSub.broadcast!(@pubsub, @topic, data)

        state =
          state
          |> Protean.Action.send({:data, data}, to: pid)

        {:reply, data, state}
      end

  """
  @callback handle_action(term(), State.t(), event) :: State.t()

  @doc """
  Optional callback to determine whether a conditional transition should occur.

  ## Example

      @machine [
        # ...
        states: [
          editing_user: [
            on: [
              {
                {:user_commit, _},
                guard: :valid_user?,
                actions: ["broadcast"],
                target: "viewing_user"
              },
              {
                {:user_commit, _},
                guard: {:not, :valid_user?},
                actions: ["show_invalid_user_error"]
              }
            ]
          ]
        ]
      ]

      @impl true
      def guard(:valid_user?, state, {_, user}) do
        User.changeset(%User{}, user).valid?
      end

  """
  @callback guard(term(), State.t(), event) :: boolean()

  @doc """
  Optional callback for defining dynamic delays.

  ## Example

      @machine [
        # ...
        states: [
          will_transition: [
            after: [
              delay: "my_delay",
              target: "new_state"
            ]
          ],
          new_state: [
            # ...
          ]
        ]
      ]

      @impl true
      def delay("my_delay", state, _) do
        state.context[:configured_delay] || 1000
      end

  """
  @callback delay(term(), State.t(), event) :: non_neg_integer()

  @optional_callbacks handle_action: 3, invoke: 3, guard: 3, delay: 3

  defmodule ConfigError do
    defexception [:message]
  end

  @spec __using__([using_option()]) :: term()
  defmacro __using__(opts \\ []) do
    unless __CALLER__.module do
      raise "`use Protean` outside of a module definition is not currently supported"
    end

    {opts, other} = Keyword.split(opts, @protean_options)

    opts =
      opts
      |> Keyword.put_new(:machine, :machine)
      |> Keyword.update(:callback_module, __CALLER__.module, fn
        {:__aliases__, _, aliases} -> Module.concat(aliases)
      end)

    unless Enum.empty?(other) do
      require Logger
      Logger.warn("unknown options passed to `use Protean`: #{inspect(other)}")
    end

    Module.put_attribute(__CALLER__.module, @protean_options_attr, opts)
    Module.register_attribute(__CALLER__.module, @protean_machine_attr, persist: true)

    quote do
      import Protean, only: [match: 1]
      @behaviour Protean
      @before_compile Protean

      def __protean_machine__ do
        __MODULE__.__info__(:attributes)
        |> Keyword.get(unquote(@protean_machine_attr))
        |> hd()
      end
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    opts = Module.get_attribute(env.module, @protean_options_attr)
    user_config = Module.get_attribute(env.module, opts[:machine])

    unless is_nil(user_config) do
      machine_config = MachineConfig.new(user_config, callback_module: opts[:callback_module])
      Module.put_attribute(env.module, @protean_machine_attr, machine_config)
    end

    [
      def_default_impls(env),
      user_config && def_default_otp()
    ]
  end

  @doc """
  Helper macro to allow match expressions on events during machine definition.

  ## Example

      @machine [
        # ...
        on: [
          # Match events that are instances of `MyStruct`
          {match(%MyStruct{}), target: "..."},

          # Match anything
          {match(_), target: "..."}
        ]
      ]
  """
  defmacro match(pattern) do
    quote(do: fn expr -> match?(unquote(pattern), expr) end)
  end

  @doc """
  Start a Protean machine linked to the current process.

  This is often used to start the machine as part of a supervision tree. See
  `GenServer.start_link/3` for description of return value.

  The semantics are similar to `GenServer.start_link/3` and accepts the same options, with the
  addition of some specific to Protean.

  ## Options

    * `:context` - context map that will be merged into the default context defined by the
      machine.
    * `:machine` - defaults to `module` - module used for machine definition.
    * `:module` - defaults to `module` - callback module used for actions, guards, invoke,
      etc. See "Callbacks".
    * `:parent` - defaults to `self()` - process id of the parent that will receive events from
      the machine if a `Protean.Action.send(..., to: :parent)` action is used or when the machine
      reaches a state with `:type` of `:final`.
    * `:supervisor` - defaults to `Protean.Supervisor` - name of the supervisor process that will
      be used to start processes resulting from running the machine. See "Supervisor".
    * Any option accepted by `GenServer.start_link/3`.

  """
  @spec start_link(module(), [start_option]) :: GenServer.on_start()
  def start_link(module, opts \\ []) do
    defaults = [
      machine: opts[:machine] || module.__protean_machine__(),
      module: module,
      parent: self(),
      supervisor: Protean.Supervisor
    ]

    Server.start_link(Keyword.merge(defaults, opts))
  end

  @doc """
  Makes a synchronous call to the machine, awaiting any transitions that result.

  Returns a tuple of `{state, replies}`, where `state` is the next state of the machine, and
  `replies` is a (possibly empty) list of replies returned by action callbacks resulting from the
  event.
  """
  @spec call(server, event, timeout()) :: {State.t(), replies :: [term()]}
  def call(protean, event, timeout \\ 5000), do: Server.call(protean, event, timeout)

  @doc """
  Sends an asynchronous event to the machine.

  Shares semantics with `GenServer.cast/2`.
  """
  @spec send(server, event) :: :ok
  def send(protean, event), do: Server.send(protean, event)

  @doc """
  Sends an event to the machine after `time` in milliseconds has passed.

  Returns a timer reference that can be canceled with `Process.cancel_timer/1`.
  """
  @spec send_after(server, event, non_neg_integer()) :: reference()
  def send_after(protean, event, time) when is_integer(time) and time >= 0 do
    Server.send_after(protean, event, time)
  end

  @doc """
  Synchronously retrieve the current machine state.

  TODO: Allow optional timeout as with `call/3`.
  """
  @spec current(server) :: State.t()
  def current(protean), do: Server.current(protean)

  @doc "TODO"
  @spec stop(server, reason :: term(), timeout()) :: :ok
  def stop(protean, reason \\ :default, timeout \\ :infinity)

  def stop(protean, :default, timeout) do
    Server.stop(protean, {:shutdown, Protean.current(protean)}, timeout)
  end

  def stop(protean, reason, timeout), do: Server.stop(protean, reason, timeout)

  @doc """
  Subscribes the caller to a running machine, returning a reference.

  Subscribers will receive messages whenever the machine transitions, as well as a `:DOWN`
  message when the machine exits. (This can be controlled with the `:monitor` option.)

  Messages are sent in the shape of:

      {:state, ref, {state, replies}}

  where:

    * `ref` is a monitor reference returned by the subscription;
    * `state` is the machine state resulting from the transition;
    * `replies` is a (possibly empty) list of replies resulting from actions on transition.

  If the process is already dead when subscribing, a `:DOWN` message is delivered immediately.

  ## Arguments

    * `server` - machine to subscribe the caller to;
    * `subscribe_to` - one of `:all` (default) or `:replies`, in which case messages will only be
      sent to the caller if the `replies` list is non-empty;
    * `options`:
      * `:monitor` - whether to receive a `:DOWN` message on receive exit (defaults to `true`).

  """
  @spec subscribe(server, subscribe_to :: term(), [subscribe_option]) :: reference()
  def subscribe(protean, subscribe_to \\ :all, opts \\ []) when is_atom(subscribe_to) do
    opts = Keyword.put_new(opts, :monitor, true)
    Server.subscribe(protean, subscribe_to, opts)
  end

  @doc "Unsubscribes the caller from the machine."
  @spec unsubscribe(server, reference()) :: :ok
  def unsubscribe(protean, ref), do: Server.unsubscribe(protean, ref)

  @doc false
  defdelegate ping(pid), to: Server

  @doc """
  Returns true if the machine is currently in the given state.

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

  # Internal helpers

  defp def_default_impls(env) do
    [
      Module.defines?(env.module, {:action, 3}, :def) &&
        quote do
          @impl Protean
          def handle_action(state, _, _), do: {:noreply, state}
        end,
      Module.defines?(env.module, {:guard, 3}, :def) &&
        quote do
          @impl Protean
          def guard(_, _, _), do: false
        end
    ]
  end

  defp def_default_otp do
    quote generated: true, location: :keep do
      def child_spec(opts) do
        {id, opts} = Keyword.pop(opts, :id, __MODULE__)

        spec = %{
          id: id,
          start: {__MODULE__, :start_link, [opts]}
        }

        Supervisor.child_spec(spec, [])
      end

      def start_link(opts \\ []) do
        Protean.start_link(__MODULE__, opts)
      end

      defoverridable child_spec: 1, start_link: 1
    end
  end
end
