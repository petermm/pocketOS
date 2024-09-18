defmodule UI do
  @bg_color 0xFFFFFF

  def start_link(args, opts) do
    :avm_scene.start_link(__MODULE__, args, opts)
  end

  def init(opts) do
    :erlang.send_after(1, self(), :show_hello)
    {:ok, Enum.into(opts, %{})}
  end

  def handle_call(_msg, _from, state) do
    {:reply, :error, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(:show_hello, %{width: width, height: height} = state) do
    items = [
      {:text, 10, 20, :default16px, 0x000000, @bg_color, "Hello."},
      {:rect, 0, 0, width, height, @bg_color}
    ]

    :erlang.send_after(3000, self(), :show_settings)

    {:noreply, state, [{:push, items}]}
  end

  def handle_info(:show_settings, %{display_server: display} = state) do
    IO.puts("settings")

    # TODO: keyboard_server might be optional, let's handle it as optional
    %{width: width, height: height, keyboard_server: keyboard_server} = state

    # TODO: same here, keyboard_server might be optional
    {:ok, settings} =
      UI.Settings.start_link(Enum.into(state, []),
        display_server: display,
        height: height,
        width: width,
        keyboard_server: keyboard_server
      )

    PhotonUI.UIServer.show(settings)

    {:noreply, state}
  end

  def handle_info(msg, state) do
    :erlang.display({:handle_info, msg})
    {:noreply, state}
  end
end
