defmodule Protean.Interpreter.Features do
  @moduledoc false

  alias Protean.Events
  alias Protean.Context
  alias Protean.Interpreter
  alias Protean.Interpreter.Hooks
  alias Protean.Interpreter.Server
  alias Protean.ProcessManager
  alias Protean.PubSub

  def install(store) do
    store
    |> Hooks.event_filter(&exit_message/1)
    |> Hooks.event_filter(&task_completed/1)
    |> Hooks.event_filter(&task_failed/1)
    |> Hooks.after_event_filter(&autoforward_event/2)
    |> Hooks.after_macrostep(&broadcast_transition/1)
    |> Hooks.on_stop(&stop_all_subprocesses/1)
  end

  defp exit_message({store, {:EXIT, _pid, _reason}}) do
    {:halt, Interpreter.stop(store)}
  end

  defp exit_message(acc), do: {:cont, acc}

  defp stop_all_subprocesses(store) do
    ProcessManager.stop_all_subprocesses()
    {:cont, store}
  end

  defp autoforward_event(store, %Events.Platform{}), do: {:cont, store}

  defp autoforward_event(store, event) do
    for {_id, pid, _ref, opts} <- ProcessManager.registered_subprocesses(),
        Keyword.get(opts, :autoforward, false) do
      Server.send(pid, event)
    end

    {:cont, store}
  end

  defp broadcast_transition(store) do
    case Context.get(store, :id) do
      nil ->
        {:cont, store}

      id ->
        replies = Context.get_replies(store)
        message = {id, store, replies}

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

        {:cont, store}
    end
  end

  defp task_completed({store, {ref, result}}) when is_reference(ref) do
    with {id, _, _, _} <- ProcessManager.subprocess_by_ref(ref),
         _ <- Process.demonitor(ref, [:flush]),
         :ok <- ProcessManager.unregister_subprocess(id) do
      event = Events.platform(:spawn, :done, id) |> Events.with_payload(result)
      {:cont, {store, event}}
    else
      nil -> {:cont, {store, {ref, result}}}
    end
  end

  defp task_completed(acc), do: {:cont, acc}

  defp task_failed({store, {:DOWN, ref, :process, _, reason} = down_event}) do
    with {id, _, _, _} <- ProcessManager.subprocess_by_ref(ref),
         :ok <- ProcessManager.unregister_subprocess(id),
         true <- otp_error?(reason) do
      event = Events.platform(:spawn, :error, id)
      {:cont, {store, event}}
    else
      nil -> {:cont, {store, down_event}}
      false -> {:halt, store}
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
