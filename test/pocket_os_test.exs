defmodule PocketOSTest do
  use ExUnit.Case
  doctest PocketOS

  test "greets the world" do
    assert PocketOS.hello() == :world
  end
end
