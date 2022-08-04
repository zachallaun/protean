defmodule Protean.Utils.Tree do
  @moduledoc false

  @doc """
  Reduce over a generic tree structure in depth-first order.

  `reducer` will be called with each node and is expected to return a 2-element
  tuple, the accumulator and a list of any children of that node.
  """
  @spec reduce(term(), acc, (term(), acc -> {acc, [term()]})) :: acc when acc: var
  def reduce(tree, acc, reducer), do: dfs_tree_reduce([tree], acc, reducer)

  defp dfs_tree_reduce([], acc, _reducer), do: acc

  defp dfs_tree_reduce([node | rest], acc, reducer) do
    {acc, children} = reducer.(node, acc)

    dfs_tree_reduce(List.wrap(children) ++ rest, acc, reducer)
  end
end
