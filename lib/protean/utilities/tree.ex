defmodule Protean.Utilities.Tree do
  @moduledoc false

  @doc """
  Reduce over a generic tree structure in depth-first order.

  `reducer` will be called with each node and is expected to return a 2-element
  tuple, the accumulator and a list of any children of that node.
  """
  @spec tree_reduce(any(), (any(), acc -> {acc, [any()]}), acc) :: acc when acc: var
  def tree_reduce(tree, reducer, acc), do: dfs_tree_reduce([tree], reducer, acc)

  defp dfs_tree_reduce(stack, reducer, acc)

  defp dfs_tree_reduce([], _reducer, acc), do: acc

  defp dfs_tree_reduce([node | rest], reducer, acc) do
    {acc, children} = reducer.(node, acc)

    dfs_tree_reduce(List.wrap(children) ++ rest, reducer, acc)
  end
end
