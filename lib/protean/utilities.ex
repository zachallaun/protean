defmodule Protean.Utilities do
  @moduledoc false

  @doc """
  Equivalent to `Enum.unzip/1` except for lists with 3-element tuples.
  """
  def unzip3(list),
    do: unzip3(Enum.reverse(list), [], [], [])

  defp unzip3([{el1, el2, el3} | reversed_list], l1, l2, l3),
    do: unzip3(reversed_list, [el1 | l1], [el2 | l2], [el3 | l3])

  defp unzip3([], l1, l2, l3),
    do: {l1, l2, l3}

  @doc """
  Equivalent to `Enum.unzip/1` except for lists with 4-element tuples.
  """
  def unzip4(list),
    do: unzip4(Enum.reverse(list), [], [], [], [])

  defp unzip4([{el1, el2, el3, el4} | reversed_list], l1, l2, l3, l4),
    do: unzip4(reversed_list, [el1 | l1], [el2 | l2], [el3 | l3], [el4 | l4])

  defp unzip4([], l1, l2, l3, l4),
    do: {l1, l2, l3, l4}
end
