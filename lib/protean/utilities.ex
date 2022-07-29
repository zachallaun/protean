defmodule Protean.Utilities do
  @moduledoc false

  @internal_prefix "$protean"

  @doc """
  Generate the event name for an internal Protean event.
  """
  def internal_event(:init) do
    "#{@internal_prefix}.init"
  end

  def internal_event(:after, node_id, delay) do
    human_id =
      node_id
      |> Enum.reverse()
      |> Enum.join(".")

    "#{@internal_prefix}.after.#{delay}-#{human_id}"
  end

  def internal_event(:invoke, :done, id), do: "#{@internal_prefix}.invoke.done-#{id}"
  def internal_event(:invoke, :error, id), do: "#{@internal_prefix}.invoke.error-#{id}"

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

  @doc """
  Generate a UUIDv4 string.
  """
  def uuid4 do
    <<u0::48, _::4, u1::12, _::2, u2::62>> = :crypto.strong_rand_bytes(16)
    <<u0::48, 4::4, u1::12, 2::2, u2::62>>
    |> uuid4_to_string()
  end

  defp uuid4_to_string(<<
        a1::4, a2::4, a3::4, a4::4, a5::4, a6::4, a7::4, a8::4,
        b1::4, b2::4, b3::4, b4::4,
        c1::4, c2::4, c3::4, c4::4,
        d1::4, d2::4, d3::4, d4::4,
        e1::4, e2::4, e3::4, e4::4, e5::4, e6::4, e7::4, e8::4, e9::4, e10::4, e11::4, e12::4
       >>) do
    <<
      e(a1), e(a2), e(a3), e(a4), e(a5), e(a6), e(a7), e(a8), ?-,
      e(b1), e(b2), e(b3), e(b4), ?-,
      e(c1), e(c2), e(c3), e(c4), ?-,
      e(d1), e(d2), e(d3), e(d4), ?-,
      e(e1), e(e2), e(e3), e(e4), e(e5), e(e6), e(e7), e(e8), e(e9), e(e10), e(e11), e(e12)
    >>
  end

  @compile {:inline, e: 1}

  defp e(0), do: ?0
  defp e(1), do: ?1
  defp e(2), do: ?2
  defp e(3), do: ?3
  defp e(4), do: ?4
  defp e(5), do: ?5
  defp e(6), do: ?6
  defp e(7), do: ?7
  defp e(8), do: ?8
  defp e(9), do: ?9
  defp e(10), do: ?a
  defp e(11), do: ?b
  defp e(12), do: ?c
  defp e(13), do: ?d
  defp e(14), do: ?e
  defp e(15), do: ?f
end
