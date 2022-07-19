defmodule Protean.Macros do
  @moduledoc false

  defmacro __using__(opts) do
    config = Keyword.fetch!(opts, :machine)

    quote location: :keep, bind_quoted: [config: config] do
      @behaviour Protean.Action.Pure
      @behaviour Protean.Action.Effect
      @behaviour Protean.Transition.Guard

      unless Module.has_attribute?(__MODULE__, :doc) do
        @doc """
        Returns a specification to start a `Protean.Interpreter` under a
        supervisor using the machine configuration defined in this module.
        """
      end

      def child_spec(opts) do
        {id, opts} = Keyword.pop(opts, :id, __MODULE__)

        defaults = [
          gen_server: [name: __MODULE__]
        ]

        spec = %{
          id: id,
          start: {__MODULE__, :start_link, [Keyword.merge(defaults, opts)]}
        }

        Supervisor.child_spec(spec, [])
      end

      defoverridable child_spec: 1

      def start_link(opts \\ []) do
        defaults = [
          handler: __MODULE__,
          machine: protean_machine()
        ]

        Protean.Interpreter.Server.start_link(Keyword.merge(defaults, opts))
      end

      defoverridable start_link: 1

      def protean_machine do
        Protean.Machine.new(unquote(Macro.escape(config)), handler: __MODULE__)
      end

      @before_compile Protean.Macros
    end
  end

  @doc false
  defmacro __before_compile__(_env) do
    quote do
      @impl Protean.Action.Pure
      def pure(_, _, _), do: nil

      @impl Protean.Action.Effect
      def effect(_, _, _), do: nil

      @impl Protean.Transition.Guard
      def condition(_, _, _, _), do: false
    end
  end
end
