defmodule Protean.Macros do
  @moduledoc """
  Injects code into the calling module in order to easily define Protean statecharts.

  This module is included by calling `use Protean` and requires that the caller define a machine
  configuration in one of three ways:

      defmodule Example1 do
        use Protean

        @machine [
          # ...
        ]
      end

      defmodule Example2 do
        use Protean, machine: :my_machine

        @my_machine [
          # ...
        ]
      end

      defmodule Example3 do
        use Protean,
          machine: [
            # ...
          ]
      end
  """

  defmodule ConfigError do
    @moduledoc false
    defexception [:message]
  end

  defmacro __using__(opts) do
    quote generated: true, location: :keep do
      @behaviour Protean.Action.Effect
      @behaviour Protean.Action.Invoke
      @behaviour Protean.Action.Pure
      @behaviour Protean.Transition.Guard

      Module.put_attribute(__MODULE__, Protean.Options, unquote(Macro.escape(opts)))

      unquote(persist_attribute(opts))

      @before_compile Protean.Macros
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    quote generated: true, location: :keep do
      unquote(def_machine_function(env))
      unquote(def_default_impls(env))
      unquote(def_default_otp(env))
    end
  end

  @doc """
  Helper macro to create a new module with the current module as the handler. Equivalent to:

      Protean.Machine.new(..., handler: __MODULE__)
  """
  defmacro machine(config) do
    quote location: :keep do
      Protean.Machine.new(unquote(config), handler: __MODULE__)
    end
  end

  defp persist_attribute(opts) do
    case Keyword.get(opts, :machine, :machine) do
      attr when is_atom(attr) ->
        quote do
          Module.register_attribute(__MODULE__, unquote(attr), persist: true)
        end

      _ ->
        :ok
    end
  end

  defp def_machine_function(env) do
    # Four cases:
    # 1. use Protean -> check for @machine or machine/0
    # 2. use Protean, machine: :foo -> check for @foo
    # 3. use Protean, machine: [foo: 0] -> check for foo/0
    # 4. use Protean, machine: [initial: ...] -> generate __protean_machine__()
    machine_option = Keyword.get(protean_opts(env), :machine)

    cond do
      is_nil(machine_option) && Module.defines?(env.module, {:machine, 0}, :def) ->
        :ok

      is_nil(machine_option) && Module.has_attribute?(env.module, :machine) ->
        quote generated: true, location: :keep do
          def unquote(machine_function_name(env))() do
            Protean.Machine.new(
              __MODULE__.__info__(:attributes)[:machine],
              handler: unquote(machine_handler_name(env))
            )
          end
        end

      match?([{_name, 0}], machine_option) ->
        [{name, 0}] = machine_option

        if Module.defines?(env.module, {name, 0}, :def) do
          :ok
        else
          raise ConfigError,
            message: "Machine function definition not found: #{to_string(name)}/0"
        end

      Keyword.keyword?(machine_option) ->
        quote generated: true, location: :keep do
          def unquote(machine_function_name(env))() do
            Protean.Machine.new(
              unquote(machine_option),
              handler: unquote(machine_handler_name(env))
            )
          end
        end

      true ->
        raise ConfigError,
          message: "No valid machine config found. Got: #{inspect(machine_option)}"
    end
  end

  defp def_default_impls(_env) do
    quote generated: true, location: :keep do
      @impl Protean.Action.Pure
      def pure(_, _, _), do: nil

      @impl Protean.Action.Effect
      def effect(_, _, _), do: nil

      @impl Protean.Action.Invoke
      def invoke(_, _, _), do: nil

      @impl Protean.Transition.Guard
      def condition(_, _, _), do: false
    end
  end

  defp def_default_otp(env) do
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
        defaults = [
          handler: unquote(machine_handler_name(env)),
          machine: unquote(machine_function_name(env))()
        ]

        Protean.Interpreter.Server.start_link(Keyword.merge(defaults, opts))
      end

      defoverridable child_spec: 1, start_link: 1
    end
  end

  defp machine_handler_name(env) do
    env
    |> protean_opts()
    |> Keyword.get(:handler, env.module)
  end

  defp machine_function_name(env) do
    case Keyword.get(protean_opts(env), :machine) do
      [{function_name, 0}] ->
        function_name

      [{function_name, n}] ->
        raise ConfigError,
          message: "machine function must have arity 0, got: #{to_string(function_name)}/#{n}"

      _other ->
        :machine
    end
  end

  defp protean_opts(env),
    do: Module.get_attribute(env.module, Protean.Options, [])
end
