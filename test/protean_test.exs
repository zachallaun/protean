defmodule ProteanTest do
  use ExUnit.Case
  doctest Protean

  test "greets the world" do
    assert Protean.hello() == :world
  end
end
