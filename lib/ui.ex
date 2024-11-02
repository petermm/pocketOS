alias PhotonUI.Widgets.ImageState

defmodule UI do
  @bg_color 0xFFFFFF

  def start_link(args, opts) do
    :avm_scene.start_link(__MODULE__, args, opts)
  end

  def init(opts) do
    Process.register(self(), :ui_server)
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

    :erlang.send_after(3000, self(), {:show, UI.Menu, []})

    {:noreply, state, [{:push, items}]}
  end

  def handle_info({:show, what, args}, %{display_server: display} = state) do
    IO.puts("settings")

    # TODO: keyboard_server might be optional, let's handle it as optional
    %{width: width, height: height, keyboard_server: keyboard_server} = state

    # TODO: same here, keyboard_server might be optional
    {:ok, {settings, _settings_ref}} =
      apply(what, :start_monitor, [
        args ++ Enum.into(state, []),
        [
          display_server: display,
          height: height,
          width: width,
          keyboard_server: keyboard_server
        ]
      ])

    PhotonUI.UIServer.show(settings)

    {:noreply, state}
  end

  defp error_description(foo) do
    {"Error:", inspect(foo)}
  end

  defp error_description(%exception_type{} = ex) do
    title = "#{exception_type}:"
    message = Exception.message(ex)
    {title, message}
  end

  def handle_info({:DOWN, _ref, :process, _pid, :normal, state}) do
    {:noreply, state}
  end

  def handle_info({:DOWN, ref, :process, pid, {err_data, _}}, state) do
    error_image = ImageState.load_image({:pocket_os, "icons/status/critical.rgba"})
    {title, message} = error_description(err_data)

    error_display = [
      {:text, 16, 16, :default16px, 0xFFFFFF, 0x404040, title},
      {:text, 16, 32, :default16px, 0xFFFFFF, 0x404040, message},
      {:text, 16, 240 - 32, :default16px, 0xFFFFFF, 0x404040, "Press any key to continue."},
      {:image, div(320 - 64, 2), div(240 - 64, 2), 0x404040, error_image},
      {:rect, 0, 0, 320, 240, 0x404040}
    ]

    {:noreply, state, [push: error_display]}
  end

  def handle_info(msg, state) do
    :erlang.display({:handle_info, msg})
    {:noreply, state}
  end
end
