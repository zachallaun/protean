defmodule Protean.TestHelper do
  @moduledoc """
  Utilities to improve Protean's internal testing. Should not be relied on externally.
  """

  @doc """
  Create a `Protean.Node` id from a more human-readable string.

  ## Examples

      iex> node_id("#")
      ["#"]

      iex> node_id("foo")
      ["foo", "#"]

      iex> node_id("foo.bar")
      ["bar", "foo", "#"]

      iex> node_id(".foo.bar")
      ["bar", "foo", "#"]

      iex> node_id("#foo.bar")
      ["bar", "foo", "#"]

      iex> node_id("#.foo.bar")
      ["bar", "foo", "#"]
  """
  def node_id(human_id) when is_binary(human_id) do
    normalized =
      human_id
      |> String.replace_prefix("#", "")
      |> String.replace_prefix(".", "")

    case normalized do
      "" -> ["#"]
      other -> other |> String.split(".") |> then(&["#" | &1]) |> Enum.reverse()
    end
  end
end
