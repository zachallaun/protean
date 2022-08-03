defmodule Protean.Macros do
  @moduledoc """
  Injects code into the calling module in order to easily define Protean statecharts.

  This module is included by calling `use Protean` and requires that the caller define a machine
  configuration using `defmachine/1`:

      defmodule Example do
        use Protean

        defmachine [
          # ...
        ]
      end

  """

  defmodule ConfigError, do: defexception([:message])

  defmacro __using__(opts) do
    quote generated: true, location: :keep do
      import Protean.Macros
      @behaviour Protean

      Module.put_attribute(__MODULE__, Protean.Options, unquote(Macro.escape(opts)))

      @before_compile Protean.Macros
    end
  end

  @doc """
  Define a Protean machine accessible through `__MODULE__.machine/0`.
  """
  defmacro defmachine(config) do
    config
    |> with_event_matchers()
    |> make_machine_function()
  end

  defp with_event_matchers(config) do
    Macro.prewalk(config, fn
      {:on, transitions} ->
        {:on,
         Enum.map(transitions, fn {pattern, transition} ->
           {make_match_fun(pattern), transition}
         end)}

      other ->
        other
    end)
  end

  defp make_match_fun(pattern) do
    quote(do: fn expr -> match?(unquote(pattern), expr) end)
  end

  defp make_machine_function(config) do
    quote location: :keep do
      def machine do
        Protean.Machine.new(unquote(config), handler: __MODULE__)
      end
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    unless Module.defines?(__CALLER__.module, {:machine, 0}, :def) do
      raise ConfigError,
        message: "Protean machine definition not found. See `Protean.Macros.defmachine/1`."
    end

    [
      def_default_impls(env),
      def_default_otp(env)
    ]
  end

  defp def_default_impls(_env) do
    quote generated: true, location: :keep do
      @impl Protean
      def action(_, _, _), do: nil

      @impl Protean
      def invoke(_, _, _), do: nil

      @impl Protean
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
          machine: machine()
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

  defp protean_opts(env) do
    Module.get_attribute(env.module, Protean.Options, [])
  end
end
