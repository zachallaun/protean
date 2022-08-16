defmodule DependentProjectTest do
  use ExUnit.Case
  doctest DependentProject

  test "greets the world" do
    assert DependentProject.hello() == :world
  end
end
