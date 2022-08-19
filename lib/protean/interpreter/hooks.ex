defmodule Protean.Interpreter.Hooks do
  @moduledoc """
  Interpreter execution hooks.

  The purpose of this module is to define extensions at various points of machine interpretation.
  See the description of the interpretation loop in `Protean.Interpreter` before reading on.

  While hooks can implement "standalone" behaviors, such as ignoring a certain type of event,
  they are used by Protean to implement higher-level features such as `:spawn`.

  See `Protean.Interpreter.Features` for more.

  ## Hooks

    * `event_filter` is used to filter out or modify events.
    * `after_event_filter` is used to perform setup that you'd only want to occur if the event
      was not filtered out.
    * `after_microstep` runs after a microstep has occurred.
    * `after_macrostep` runs after a macrostep has occurred.
    * `on_stop` runs when interpretation stops.

  See function documentation for the individual hooks below.
  """

  alias Protean.Interpreter

  @doc """
  Register an `event_filter` hook.

  Function must accept one argument, `{interpreter, event}` and return one of:

    * `{:cont, {interpreter, event}}` - continue with interpreter and event
    * `{:halt, interpreter}` - halt with interpreter and ignore event

  """
  def event_filter(%Interpreter{} = interpreter, hook) when is_function(hook, 1) do
    append_hook(interpreter, :event_filter, hook)
  end

  @doc """
  Register an `after_event_fitler` hook.

  Function must accept two arguments, `interpreter` and `event`, and must return
  {:cont, `interpreter`}.
  """
  def after_event_filter(%Interpreter{} = interpreter, hook) when is_function(hook, 2) do
    append_hook(interpreter, :after_event_filter, hook)
  end

  @doc """
  Register an `after_microstep` hook.

  Function must accept `interpreter` and return {:cont, `interpreter`}.
  """
  def after_microstep(%Interpreter{} = interpreter, hook) when is_function(hook, 1) do
    append_hook(interpreter, :after_microstep, hook)
  end

  @doc """
  Register an `after_macrostep` hook.

  Function must accept an `interpreter` and return {:cont, `interpreter`}.
  """
  def after_macrostep(%Interpreter{} = interpreter, hook) when is_function(hook, 1) do
    append_hook(interpreter, :after_macrostep, hook)
  end

  @doc """
  Register an `on_stop` hook.

  Function must accept an `interpreter` and return {:cont, `interpreter`}.
  """
  def on_stop(%Interpreter{} = interpreter, hook) when is_function(hook, 1) do
    append_hook(interpreter, :on_stop, hook)
  end

  ## Interpreter callbacks

  @doc false
  def run(interpreter, :event_filter, event) do
    interpreter
    |> get_hooks(:event_filter)
    |> run_hooks({interpreter, event})
  end

  def run(interpreter, :after_event_filter, event) do
    interpreter
    |> get_hooks(:after_event_filter)
    |> run_hooks(interpreter, [event])
  end

  @doc false
  def run(interpreter, hook) do
    interpreter
    |> get_hooks(hook)
    |> run_hooks(interpreter)
  end

  ## Utilities

  defp run_hooks(hooks, acc, extra_args \\ []) do
    Enum.reduce_while(hooks, acc, fn hook, acc ->
      apply(hook, [acc | extra_args])
    end)
  end

  defp get_hooks(interpreter, hook_type) do
    Map.get(interpreter.hooks, hook_type, [])
  end

  defp append_hook(interpreter, hook_type, hook) do
    %{interpreter | hooks: Map.update(interpreter.hooks, hook_type, [hook], &(&1 ++ [hook]))}
  end
end
