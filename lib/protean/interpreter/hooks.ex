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

  alias Protean.Context

  @doc """
  Register an `event_filter` hook.

  Function must accept one argument, `{interpreter, event}` and return one of:

    * `{:cont, {interpreter, event}}` - continue with interpreter and event
    * `{:halt, interpreter}` - halt with interpreter and ignore event

  """
  def event_filter(store, hook) when is_function(hook, 1) do
    append_hook(store, :event_filter, hook)
  end

  @doc """
  Register an `after_event_fitler` hook.

  Function must accept two arguments, `interpreter` and `event`, and must return
  {:cont, `interpreter`}.
  """
  def after_event_filter(store, hook) when is_function(hook, 2) do
    append_hook(store, :after_event_filter, hook)
  end

  @doc """
  Register an `after_microstep` hook.

  Function must accept `interpreter` and return {:cont, `interpreter`}.
  """
  def after_microstep(store, hook) when is_function(hook, 1) do
    append_hook(store, :after_microstep, hook)
  end

  @doc """
  Register an `after_macrostep` hook.

  Function must accept an `interpreter` and return {:cont, `interpreter`}.
  """
  def after_macrostep(store, hook) when is_function(hook, 1) do
    append_hook(store, :after_macrostep, hook)
  end

  @doc """
  Register an `on_stop` hook.

  Function must accept an `interpreter` and return {:cont, `interpreter`}.
  """
  def on_stop(store, hook) when is_function(hook, 1) do
    append_hook(store, :on_stop, hook)
  end

  ## Interpreter callbacks

  @doc false
  def run(store, :event_filter, event) do
    store
    |> get_hooks(:event_filter)
    |> run_hooks({store, event})
  end

  def run(store, :after_event_filter, event) do
    store
    |> get_hooks(:after_event_filter)
    |> run_hooks(store, [event])
  end

  @doc false
  def run(store, hook) do
    store
    |> get_hooks(hook)
    |> run_hooks(store)
  end

  ## Utilities

  defp run_hooks(hooks, acc, extra_args \\ []) do
    Enum.reduce_while(hooks, acc, fn hook, acc ->
      apply(hook, [acc | extra_args])
    end)
  end

  defp get_hooks(store, hook_type) do
    store
    |> Context.get(:hooks)
    |> Map.get(hook_type, [])
  end

  defp append_hook(store, hook_type, hook) do
    Context.update(store, :hooks, fn hooks ->
      Map.update(hooks, hook_type, [hook], &(&1 ++ [hook]))
    end)
  end
end
