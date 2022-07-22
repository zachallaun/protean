defmodule Protean.Action.Invoke do
  @moduledoc """
  Handles the creation of invoked services. An invoked service is anything that can be started
  by a `DynamicSupervisor` with `DynamicSupervisor.start_child/2`.

  This action is not usually used directly, but instead is used under-the-hood in machine
  configurations that use `:invoke`.
  """

  alias __MODULE__
  alias Protean.Action.Protocol.Executable
  alias Protean.Action.Protocol.Resolvable
  alias Protean.Utilities

  defmodule Resolved do
    @enforce_keys [:id, :child_spec_fun]

    defstruct [
      :id,
      :child_spec_fun
    ]

    defimpl Executable, for: __MODULE__ do
      def exec(%{id: id, child_spec_fun: child_spec_fun}, interpreter) do
        case DynamicSupervisor.start_child(Protean.Supervisor, child_spec_fun.(self())) do
          {:ok, child} ->
            ref = Process.monitor(child)

            update_in(
              interpreter.invoked,
              &Map.put(&1, id, %{id: id, pid: child, ref: ref, autoforward: false})
            )
        end
      end
    end
  end

  defmodule Resolved.Cancel do
    defstruct [:id]

    defimpl Resolvable, for: __MODULE__ do
      def resolve(self, _state, _handler), do: self
    end

    defimpl Executable, for: __MODULE__ do
      def exec(%{id: id}, interpreter) do
        case interpreter.invoked[id] do
          %{pid: pid, ref: ref} ->
            DynamicSupervisor.terminate_child(Protean.Supervisor, pid)
            Process.demonitor(ref, [:flush])
            update_in(interpreter.invoked, &Map.delete(&1, id))

          _ ->
            interpreter
        end
      end
    end
  end

  defmodule Unresolved.Task do
    defstruct [
      :id,
      :task,
      :event_name
    ]

    defimpl Resolvable, for: __MODULE__ do
      def resolve(%{id: id, task: task, event_name: event_name}, state, handler)
          when is_binary(task) do
        task
        |> handler.invoke(state, state.event)
        |> resolved(id, event_name)
      end

      def resolve(%{id: id, task: task, event_name: event_name}, _state, _handler) do
        resolved(task, id, event_name)
      end

      defp resolved(task, id, event_name) do
        %Invoke.Resolved{
          id: id,
          child_spec_fun: fn pid ->
            Task.child_spec(fn -> Unresolved.Task.run_task(task, pid, event_name) end)
          end
        }
      end
    end

    @doc false
    def run_task({m, f, a}, to, event_name) do
      apply(m, f, a)
      |> send_result_as_event(to, event_name)
    end

    def run_task(f, to, event_name) when is_function(f) do
      f.()
      |> send_result_as_event(to, event_name)
    end

    defp send_result_as_event(result, to, event_name) do
      Protean.send_event(to, {event_name, result})
    end
  end

  def task(id, task) do
    %Unresolved.Task{
      id: id,
      task: task,
      event_name: Utilities.internal_event(:invoke, :done, id)
    }
  end

  def delayed_send(event_name, delay) do
    %Unresolved.Task{
      id: event_name,
      event_name: event_name,
      task: {__MODULE__, :delay, [delay]}
    }
  end

  def cancel(id), do: %Resolved.Cancel{id: id}

  @doc false
  def delay(milliseconds), do: :timer.sleep(milliseconds)
end
