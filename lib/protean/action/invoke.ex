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
  alias Protean.State
  alias Protean.Utilities

  @doc """
  Called with handler name and machine state. Returns a child spec usable by the invoke type used
  to register the handler name.
  """
  @callback invoke(String.t(), State.t()) :: any

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

        # TODO:
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
      :task
    ]

    defimpl Resolvable, for: __MODULE__ do
      def resolve(%{id: id, task: task}, state, handler) when is_binary(task) do
        task
        |> handler.invoke(state)
        |> resolved(id)
      end

      def resolve(%{id: id, task: task}, _state, _handler) do
        resolved(task, id)
      end

      defp resolved(task, id) do
        %Invoke.Resolved{
          id: id,
          child_spec_fun: fn pid ->
            Task.child_spec(fn -> Unresolved.Task.run_task(task, id, pid) end)
          end
        }
      end
    end

    @doc false
    def run_task({m, f, a}, id, send_to) do
      apply(m, f, a)
      |> Invoke.send_done_event(id, send_to)
    end

    def run_task(f, id, send_to) when is_function(f) do
      f.()
      |> Invoke.send_done_event(id, send_to)
    end
  end

  def task(id, task),
    do: %Unresolved.Task{id: id, task: task}

  def cancel_invoke(id),
    do: %Resolved.Cancel{id: id}

  @doc false
  def send_done_event(result, id, to) do
    event = {Utilities.internal_event(:invoke, :done, id), result}
    Protean.send_event(to, event)
  end
end
