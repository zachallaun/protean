defmodule Protean.PubSub do
  @moduledoc false

  @pubsub Module.concat(__MODULE__, Adapter)

  def name, do: @pubsub

  @spec subscribe(binary()) :: :ok | {:error, term()}
  def subscribe(topic) do
    Phoenix.PubSub.subscribe(@pubsub, topic)
  end

  @spec subscribe(binary(), term()) :: :ok | {:error, term()}
  def subscribe(topic, filter) do
    Phoenix.PubSub.subscribe(@pubsub, topic, metadata: {:filter, filter})
  end

  @spec unsubscribe(binary()) :: :ok
  def unsubscribe(topic) do
    Phoenix.PubSub.unsubscribe(@pubsub, topic)
  end

  @spec broadcast(binary(), term(), term()) :: :ok | {:error, term()}
  def broadcast(topic, message, filter) do
    Phoenix.PubSub.broadcast(@pubsub, topic, {message, filter}, __MODULE__)
  end

  # Phoenix.PubSub dispatch hook
  def dispatch(subscribers, :none, {message, filter}) do
    Enum.each(subscribers, fn
      {pid, {:filter, ^filter}} -> send(pid, message)
      {_pid, {:filter, _}} -> :ok
      {pid, _} -> send(pid, message)
    end)

    :ok
  end
end
