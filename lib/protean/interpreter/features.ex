defmodule Protean.Interpreter.Features do
  @moduledoc "TODO"

  alias Protean.Events
  alias Protean.Context
  alias Protean.Interpreter
  alias Protean.Interpreter.Hooks
  alias Protean.Interpreter.Server
  alias Protean.ProcessManager
  alias Protean.PubSub

  @dialyzer {:nowarn_function, install_debug: 2}
  @debug false

  def install(interpreter) do
    interpreter
    |> install_debug(@debug)
    |> Hooks.event_filter(&exit_message/1)
    |> Hooks.event_filter(&task_completed/1)
    |> Hooks.event_filter(&task_failed/1)
    |> Hooks.after_event_filter(&autoforward_event/2)
    |> Hooks.after_macrostep(&broadcast_transition/1)
    |> Hooks.on_stop(&stop_all_subprocesses/1)
  end

  defp install_debug(interpreter, false), do: interpreter

  defp install_debug(interpreter, _) do
    interpreter
    |> Hooks.event_filter(&log/1)
    |> Hooks.after_event_filter(&log/2)
  end

  defp log({interpreter, event}) do
    require Logger
    Logger.info(inspect([:before, module: interpreter.config.callback_module, event: event]))
    {:cont, {interpreter, event}}
  end

  defp log(interpreter, event) do
    require Logger
    Logger.info(inspect([:after, module: interpreter.config.callback_module, event: event]))
    {:cont, interpreter}
  end

  defp exit_message({interpreter, {:EXIT, _pid, _reason}}) do
    {:halt, Interpreter.stop(interpreter)}
  end

  defp exit_message(acc), do: {:cont, acc}

  defp stop_all_subprocesses(interpreter) do
    ProcessManager.stop_all_subprocesses()
    {:cont, interpreter}
  end

  defp autoforward_event(interpreter, %Events.Platform{}), do: {:cont, interpreter}

  defp autoforward_event(interpreter, event) do
    for {_id, pid, _ref, opts} <- ProcessManager.registered_subprocesses(),
        Keyword.get(opts, :autoforward, false) do
      Server.send(pid, event)
    end

    {:cont, interpreter}
  end

  defp broadcast_transition(%Interpreter{id: nil} = interpreter), do: {:cont, interpreter}

  defp broadcast_transition(interpreter) do
    %{id: id, context: context} = interpreter
    replies = Context.get_replies(context)
    message = {id, context, replies}

    if Enum.empty?(replies) do
      PubSub.broadcast(id, message, nil)
    else
      PubSub.broadcast(id, message, :replies)
    end
    |> case do
      :ok ->
        :ok

      {:error, error} ->
        require Logger
        Logger.warn("PubSub broadcast error: #{inspect(error)}")
    end

    {:cont, interpreter}
  end

  defp task_completed({interpreter, {ref, result}}) when is_reference(ref) do
    with {id, _, _, _} <- ProcessManager.subprocess_by_ref(ref),
         _ <- Process.demonitor(ref, [:flush]),
         :ok <- ProcessManager.unregister_subprocess(id) do
      event = Events.platform(:spawn, :done, id) |> Events.with_payload(result)
      {:cont, {interpreter, event}}
    else
      nil -> {:cont, {interpreter, {ref, result}}}
    end
  end

  defp task_completed(acc), do: {:cont, acc}

  defp task_failed({interpreter, {:DOWN, ref, :process, _, reason} = down_event}) do
    with {id, _, _, _} <- ProcessManager.subprocess_by_ref(ref),
         :ok <- ProcessManager.unregister_subprocess(id),
         true <- otp_error?(reason) do
      event = Events.platform(:spawn, :error, id)
      {:cont, {interpreter, event}}
    else
      nil -> {:cont, {interpreter, down_event}}
      false -> {:halt, interpreter}
    end
  end

  defp task_failed(acc), do: {:cont, acc}

  defp otp_error?(reason) do
    case reason do
      :normal -> false
      :shutdown -> false
      {:shutdown, _} -> false
      _ -> true
    end
  end
end
