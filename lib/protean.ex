defmodule Protean do
  @external_resource "README.md"
  @moduledoc "README.md"
             |> File.read!()
             |> String.split("<!-- MDOC !-->")
             |> Enum.fetch!(1)

  import Kernel, except: [send: 2]

  alias Protean.Context
  alias Protean.Interpreter.Server
  alias Protean.MachineConfig
  alias Protean.ProcessManager
  alias Protean.PubSub
  alias Protean.Utils

  @typedoc "A running Protean machine process."
  @type server :: GenServer.server()

  @typedoc "Unique identifier for a Protean machine process."
  @type id :: binary()

  @typedoc "Any message sent to a Protean machine."
  @type event :: term()

  @typedoc "Option values for `start*` functions."
  @type start_option :: machine_option | GenServer.option()

  @typedoc "Return values of `start_machine/2`"
  @type on_start :: {:ok, server, id} | :ignore | {:error, {:already_started, server} | term()}

  @typedoc "Option values for Protean machines."
  @type machine_option ::
          {:assigns, Context.assigns()}
          | {:supervisor, Supervisor.name()}
          | {:machine, MachineConfig.t()}
          | {:module, module()}
          | {:parent, server | pid()}

  @typedoc "Option values for `use Protean`."
  @type using_option :: {:callback_module, module()}

  @type invoke_type :: :delegate | :proc | :task | :stream

  @protean_options [:machine, :callback_module]
  @protean_options_attr :"$protean.options"
  @protean_machine_attr :"$protean.machine"

  @doc """
  Optional callback for invoked processes specified during machine execution.

  Returns a tuple of `{invoke_type, child_spec}`.

  ## Example

      @machine [
        # ...
        states: [
          # ...
          awaiting_task: [
            invoke: [
              delegate: "my_task",
              done: "completed"
            ]
          ],
          completed: [
            # ...
          ]
        ]
      ]

      @impl true
      def invoke("my_task", _context, event_data) do
        {:task, {__MODULE__, :run_my_task, [event_data]}}
      end

  """
  @callback invoke(term(), Context.t(), event) :: {invoke_type, term()}

  @doc """
  Optional callback for actions specified in response to a transition.

  Receives the current machine context and event triggering the action as arguments. Returns one
  of:

    * `context` - same as `{:noreply, context}`
    * `{:noreply, context}` - the machine context with any new actions
    * `{:reply, reply, context}` - a reply and the machine context with any new actions

  ## Example

      @machine [
        # ...
        on: [
          match({:data, _any},
            target: :data_received,
            actions: [:assign_data, :broadcast_data]
          )
        ]
      ]

      @impl true
      def handle_action(:assign_data, context, {:data, data}) do
        context
        |> Protean.Action.assign(:last_received, data)
      end

      def handle_action(:broadcast_data, context, _) do
        %{notify: pid, last_received: data} = context.assigns

        PubSub.broadcast!(@pubsub, @topic, data)

        context =
          context
          |> Protean.Action.send({:data, data}, to: pid)

        {:reply, data, context}
      end

  """
  @callback handle_action(term(), Context.t(), event) :: Context.t()

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
      def guard(:valid_user?, context, {_, user}) do
        User.changeset(%User{}, user).valid?
      end

  """
  @callback guard(term(), Context.t(), event) :: boolean()

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
      def delay("my_delay", context, _) do
        context.assigns[:configured_delay] || 1000
      end

  """
  @callback delay(term(), Context.t(), event) :: non_neg_integer()

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
      import Protean.Builder
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
  Start a Protean machine linked to Protean's supervisor.

  By default, machines will be registered and named using Protean's process management registry.

  ## Options

    * `:assigns` - assigns map that will be merged into the default machine context.
    * `:machine` - defaults to the machine defined in `module` - machine configuration.
    * `:module` - defaults to `module` - callback module used for actions, guards, invoke,
      etc. See "Callbacks".
    * `:parent` - defaults to `self()` - process id of the parent that will receive events from
      the machine if a `Protean.Action.send(..., to: :parent)` action is used or when the machine
      reaches a state with `:type` of `:final`.
    * Any option accepted by `GenServer.start_link/3`.

  """
  @spec start_machine(module(), [start_option]) :: on_start
  def start_machine(module, opts \\ []) do
    id = Utils.uuid4()
    supplied_name? = Keyword.has_key?(opts, :name)

    opts =
      opts
      |> Keyword.put(:id, id)
      |> Keyword.put_new(:name, ProcessManager.via_registry({module, id}))

    module
    |> child_spec(opts)
    |> Supervisor.child_spec(id: opts[:name], restart: :transient)
    |> ProcessManager.start_child()
    |> case do
      {:ok, pid} ->
        if supplied_name? do
          {:ok, pid, id}
        else
          {:ok, opts[:name], id}
        end

      other ->
        other
    end
  end

  @doc false
  def child_spec(module, opts) do
    defaults = [
      machine: opts[:machine] || module.__protean_machine__(),
      module: module,
      parent: self()
    ]

    %{id: module, start: {Server, :start_link, [Keyword.merge(defaults, opts)]}}
  end

  @doc """
  Makes a synchronous call to the machine, awaiting any transitions that result.

  Returns a tuple of `{context, replies}`, where `context` is the next state of the machine, and
  `replies` is a (possibly empty) list of replies returned by action callbacks resulting from the
  event.
  """
  @spec call(server, event, timeout()) :: {Context.t(), replies :: [term()]}
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
  Synchronously retrieve the current machine context.

  TODO: Allow optional timeout as with `call/3`.
  """
  @spec current(server) :: Context.t()
  def current(protean), do: Server.current(protean)

  @doc "TODO"
  @spec stop(server, reason :: term(), timeout()) :: :ok
  def stop(protean, reason \\ :default, timeout \\ :infinity)

  def stop(protean, :default, timeout) do
    Server.stop(protean, {:shutdown, Protean.current(protean)}, timeout)
  end

  def stop(protean, reason, timeout), do: Server.stop(protean, reason, timeout)

  @doc """
  Subscribes the caller to receive messages when a machine transitions.

    * `id` - id of the machine to subscribe to.

  Options:

    * `:filter` - if set to `:replies`, the caller will only be sent messages with replies.

  Messages are sent in the shape:

      {id, context, replies}

  Subscriptions depend on `Phoenix.PubSub`, an optional dependency.
  """
  @spec subscribe(id, [{:filter, :replies}]) :: :ok | {:error, term()}
  def subscribe(id, opts \\ []) when is_list(opts) do
    case Keyword.fetch(opts, :filter) do
      :error -> PubSub.subscribe(id)
      {:ok, :replies} -> PubSub.subscribe(id, :replies)
      {:ok, other} -> raise "unknown filter #{inspect(other)}"
    end
  end

  @doc """
  Unsubscribes the caller from machine transition messages.
  """
  @spec unsubscribe(id) :: :ok
  def unsubscribe(id), do: PubSub.unsubscribe(id)

  @doc """
  Returns true if the machine is currently in the given state.

  Note that calling `matches?/2` on a machine process is a synchronous operation that is
  equivalent to:

      machine |> Protean.current() |> Protean.matches?(descriptor)

  """
  @spec matches?(Context.t(), descriptor :: term()) :: boolean()
  @spec matches?(server, descriptor :: term()) :: boolean()
  def matches?(item, descriptor)

  def matches?(%Context{} = context, descriptor) do
    Context.matches?(context, descriptor)
  end

  def matches?(protean, descriptor) do
    Server.matches?(protean, descriptor)
  end

  @doc false
  defdelegate ping(pid), to: Server

  # Internal helpers

  defp def_default_impls(env) do
    [
      Module.defines?(env.module, {:action, 3}, :def) &&
        quote do
          @impl Protean
          def handle_action(context, _, _), do: {:noreply, context}
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
        Protean.child_spec(__MODULE__, opts)
      end

      defoverridable child_spec: 1
    end
  end
end
