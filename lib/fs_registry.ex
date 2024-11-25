defmodule FSRegistry do
  def register_fs(fs_name, pid) do
    :gen_server.call(__MODULE__, {:register_fs, fs_name, pid})
  end

  def whereis(fs_name) do
    :gen_server.call(__MODULE__, {:whereis, fs_name})
  end

  def start_link() do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, [], [])
  end

  def init([]) do
    {:ok, %{fs: %{}}}
  end

  def handle_call({:register_fs, fs_name, pid}, _from, %{fs: fs} = state) do
    {:reply, :ok, %{state | fs: Map.put(fs, fs_name, pid)}}
  end

  def handle_call({:whereis, fs_name}, _from, state) do
    case state do
      %{fs: %{^fs_name => pid}} -> {:reply, {:ok, pid}, state}
      _ -> {:reply, {:error, :not_found}, state}
    end
  end

  def handle_cast(_msg, state) do
    {:reply, :error, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
