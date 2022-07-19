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

  require Logger

  defmacro __using__(opts) do
    quote generated: true, location: :keep do
      @behaviour Protean.Action.Pure
      @behaviour Protean.Action.Effect
      @behaviour Protean.Transition.Guard

      Module.put_attribute(__MODULE__, Protean.Options, unquote(opts))

      @before_compile Protean.Macros
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    case resolve_machine(env) do
      machine when is_list(machine) ->
        def_defaults(env, machine)

      nil ->
        Logger.error(
          "Protean: machine config not found. Please define `@machine` or pass machine to `use Protean, machine: [...]`"
        )

      other ->
        Logger.error("Protean: machine config must be a keyword list. Got #{inspect(other)}")
    end
  end

  defp def_defaults(env, machine) do
    quote generated: true, location: :keep do
      def unquote(machine_function_name(env))() do
        Protean.Machine.new(
          unquote(Macro.escape(machine)),
          handler: unquote(machine_handler_name(env))
        )
      end

      unquote(def_default_impls(env))
      unquote(def_default_otp(env))
    end
  end

  defp def_default_impls(_env) do
    quote generated: true, location: :keep do
      @impl Protean.Action.Pure
      def pure(_, _, _), do: nil

      @impl Protean.Action.Effect
      def effect(_, _, _), do: nil

      @impl Protean.Transition.Guard
      def condition(_, _, _, _), do: false
    end
  end

  defp def_default_otp(env) do
    quote generated: true, location: :keep do
      def child_spec(opts) do
        {id, opts} = Keyword.pop(opts, :id, __MODULE__)

        spec = %{
          id: id,
          start: {__MODULE__, :start_link, [Keyword.merge([name: __MODULE__], opts)]}
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

  defp resolve_machine(env) do
    opts = Module.get_attribute(env.module, Protean.Options)

    case Keyword.get(opts, :machine, :machine) do
      attr when is_atom(attr) ->
        Module.get_attribute(env.module, attr)

      machine ->
        machine
    end
  end

  defp machine_handler_name(env) do
    env.module
    |> Module.get_attribute(Protean.Options)
    |> Keyword.get(:handler, env.module)
  end

  defp machine_function_name(_env) do
    :__protean_machine__
  end
end
