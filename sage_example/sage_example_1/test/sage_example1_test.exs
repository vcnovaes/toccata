defmodule SageExample1Test do
  use ExUnit.Case
  doctest SageExample1

  test "greets the world" do
    assert SageExample1.hello() == :world
  end
end
