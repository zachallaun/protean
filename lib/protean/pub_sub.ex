defmodule Protean.PubSub do
  @moduledoc false

  @compile {:inline, pubsub_name: 0}
  def child_spec(_) do
    configure!()

    %{
      id: pubsub_name(),
      start: {__MODULE__, :start_link, []}
    }
  end

  # Phoenix.PubSub dispatch hook
  def dispatch(subscribers, :none, {message, filter}) do
    Enum.each(subscribers, fn
      {pid, {:filter, ^filter}} -> send(pid, message)
      {pid, {:filter, nil}} -> send(pid, message)
      _ -> :ok
    end)

    :ok
  end

  defp pubsub_name do
    :persistent_term.get({__MODULE__, :name})
  end

  defp configure! do
    name =
      :protean
      |> Application.get_env(:pubsub, [])
      |> Keyword.fetch!(:name)

    :persistent_term.put({__MODULE__, :name}, name)
  end

  if Code.ensure_loaded?(Phoenix.PubSub) do
    def start_link do
      if start?() do
        Phoenix.PubSub.Supervisor.start_link(name: pubsub_name())
      else
        :ignore
      end
    end

    @spec subscribe(binary(), term()) :: :ok | {:error, {:already_registered, pid()}}
    def subscribe(topic, filter) do
      Phoenix.PubSub.subscribe(pubsub_name(), topic, metadata: {:filter, filter})
    end

    def subscribe(topic) do
      Phoenix.PubSub.subscribe(pubsub_name(), topic)
    end

    @spec unsubscribe(binary()) :: :ok
    def unsubscribe(topic) do
      Phoenix.PubSub.unsubscribe(pubsub_name(), topic)
    end

    @spec broadcast(binary(), term(), term()) :: :ok | {:error, term()}
    def broadcast(topic, message, filter) do
      Phoenix.PubSub.broadcast(pubsub_name(), topic, {message, filter}, __MODULE__)
    end

    defp start? do
      :protean
      |> Application.get_env(:pubsub, [])
      |> Keyword.get(:start, false)
    end
  else
    def start_link, do: :ignore
    def broadcast(_, _, _), do: :ok
    def unsubscribe(_), do: :ok

    def subscribe(_, _ \\ nil) do
      raise ArgumentError, """
      Protean subscriptions depend on `:phoenix_pubsub`, which must be added to your \
      application dependencies.
      """
    end
  end
end
