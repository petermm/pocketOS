defmodule StackedFSTest do
  use ExUnit.Case

  test "stacked fs basic usage" do
    {:ok, pid} = StackedFS.start_link("./")
    {:ok, file} = StackedFS.open(pid, "mix.exs", [:read])

    assert StackedFS.read(file, 9) == {:ok, "defmodule"}

    assert StackedFS.close(file) == :ok
  end
end
