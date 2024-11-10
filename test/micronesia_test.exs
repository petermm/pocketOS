defmodule MicronesiaTest do
  use ExUnit.Case

  test "micronesia basic usage" do
    assert :micronesia.start() == :ok
    assert :micronesia.create_table(Foo) == {:atomic, :ok}
    assert :micronesia.dirty_write({Foo, 42, :hello}) == :ok
    assert :micronesia.dirty_write({Foo, 43, :ciao}) == :ok
    assert :micronesia.dirty_read({Foo, 42}) == [{Foo, 42, :hello}]
    assert length(:micronesia.all(Foo)) == 2
    assert :micronesia.delete_table(Foo) == {:atomic, :ok}
  end

  test "micronesia update record" do
    assert :micronesia.start() == :ok
    :micronesia.create_table(UpdateTest)
    assert :micronesia.dirty_write({UpdateTest, 42, :hello}) == :ok
    assert :micronesia.dirty_read({UpdateTest, 42}) == [{UpdateTest, 42, :hello}]
    assert :micronesia.dirty_write({UpdateTest, 42, :ciao}) == :ok
    assert :micronesia.dirty_read({UpdateTest, 42}) == [{UpdateTest, 42, :ciao}]
    assert length(:micronesia.all(UpdateTest)) == 1
    assert :micronesia.delete_table(UpdateTest) == {:atomic, :ok}
  end

  test "micronesia subscribe table" do
    this_pid = self()

    assert :micronesia.start() == :ok
    assert :micronesia.create_table(SubscribeTest) == {:atomic, :ok}
    assert :micronesia.subscribe({:table, SubscribeTest, :simple}) == {:ok, :nonode@nohost}

    assert :micronesia.subscribe({:table, SubscribeTest, :simple}) ==
             {:error, {:already_exists, {:table, SubscribeTest, :simple}}}

    parent = self()

    spawn(fn ->
      :micronesia.subscribe({:table, SubscribeTest, :simple})
      send(parent, :done)
    end)

    assert_receive(:done, 5000)

    assert :micronesia.dirty_write({SubscribeTest, 42, :hello}) == :ok

    assert_receive(
      {:mnesia_table_event, {:write, {SubscribeTest, 42, :hello}, {:dirty, ^this_pid}}},
      5000
    )

    assert :micronesia.dirty_write({SubscribeTest, 43, :world}) == :ok

    assert_receive(
      {:mnesia_table_event, {:write, {SubscribeTest, 43, :world}, {:dirty, ^this_pid}}},
      5000
    )

    assert :micronesia.dirty_write({SubscribeTest, 42, :ciao}) == :ok

    assert_receive(
      {:mnesia_table_event, {:write, {SubscribeTest, 42, :ciao}, {:dirty, ^this_pid}}},
      5000
    )

    assert :micronesia.unsubscribe({:table, SubscribeTest, :simple}) == {:ok, :nonode@nohost}

    assert :micronesia.delete_table(SubscribeTest) == {:atomic, :ok}

    assert :micronesia.unsubscribe({:table, SubscribeTest, :simple}) == {:error, :badarg}
  end
end
