defmodule HAL do
  @compile {:no_warn_undefined, :spi}

  # @platform "linux"
  # @platform "esp32-devkit"
  @platform {"m5stack", "faces"}

  def init() do
    IO.puts("Platform is: #{inspect(@platform)}")
    init(@platform)
  end

  def init("linux") do
    open_sdl_display()
  end

  def init(platform) when platform in [{"m5stack", "faces"}, "esp32-devkit"] do
    open_ili9342c_display(platform)
  end

  def init("t-deck") do
    :ledc.timer_config(
      duty_resolution: 8,
      timer_num: 0,
      freq_hz: 1000,
      speed_mode: 0
    )

    :ledc.channel_config(
      channel: 0,
      speed_mode: 0,
      duty: 191,
      gpio_num: 42,
      timer_sel: 0,
      hpoint: 0
    )

    open_ili9342c_display("t-deck")
  end

  defp open_sdl_display do
    display_opts = [
      width: 320,
      height: 240
    ]

    case :erlang.open_port({:spawn, "display"}, display_opts) do
      display when is_port(display) ->
        {:ok,
         %{
           display: %{
             display_server: {:port, display},
             width: display_opts[:width],
             height: display_opts[:height]
           }
         }}

      _ ->
        IO.puts("Failed to open display")
        :error
    end
  end

  defp get_spi_display_opts("esp32-devkit") do
    [
      width: 320,
      height: 240,
      compatible: "ilitek,ili9341",
      reset: 18,
      cs: 22,
      dc: 21,
      backlight: 5,
      backlight_active: :low,
      backlight_enabled: true,
      rotation: 1,
      enable_tft_invon: false
    ]
  end

  defp get_spi_display_opts({"m5stack", "faces"}) do
    [
      width: 320,
      height: 240,
      compatible: "ilitek,ili9341",
      reset: 33,
      cs: 14,
      dc: 27,
      backlight: 32,
      backlight_enabled: true,
      rotation: 0,
      enable_tft_invon: true
    ]
  end

  defp get_spi_display_opts("t-deck") do
    [
      width: 320,
      height: 240,
      compatible: "sitronix,st7789",
      cs: 12,
      dc: 11,
      rotation: 1,
      enable_tft_invon: true,
      init_seq_type: "alt_gamma_2"
    ]
  end

  def get_input_devices({"m5stack", "faces"}) do
    {:ok, face} = :face.start_link()
    :ok = :gen_server.call(face, :open)
    :ok = :gen_server.call(face, {:subscribe_input, :all})
    [keyboard_server: [face]]
  end

  def get_input_devices("t-deck") do
    {:ok, face} = :polled_keyboard.start_link()
    :ok = :gen_server.call(face, :open)
    :ok = :gen_server.call(face, {:subscribe_input, :all})

    {:ok, buttons} = :buttons.start_link()

    :ok =
      :gen_server.call(
        buttons,
        {:open, %{0 => :central, 1 => :left, 2 => :right, 3 => :up, 15 => :down}}
      )

    :ok = :gen_server.call(buttons, {:subscribe_input, :all})

    [keyboard_server: [face, buttons]]
  end

  def get_input_devices(_) do
    []
  end

  defp open_display_spi_host("esp32-devkit") do
    spi_opts = %{
      bus_config: %{sclk: 19, mosi: 23, miso: 25, peripheral: "spi2"},
      device_config: %{}
    }

    :spi.open(spi_opts)
  end

  defp open_display_spi_host("t-deck") do
    spi_opts = %{
      bus_config: %{sclk: 40, mosi: 41, miso: 38, peripheral: "spi2"},
      device_config: %{}
    }

    :spi.open(spi_opts)
  end

  defp open_display_spi_host({"m5stack", "faces"}) do
    spi_opts = %{
      bus_config: %{mosi: 23, sclk: 18, peripheral: "spi2"},
      device_config: %{}
    }

    :spi.open(spi_opts)
  end

  defp open_ili9342c_display(platform) do
    spi_host = open_display_spi_host(platform)

    spi_display_opts =
      [spi_host: spi_host] ++ get_spi_display_opts(platform)

    case :erlang.open_port({:spawn, "display"}, spi_display_opts) do
      display when is_port(display) ->
        {:ok,
         %{
           display: %{
             display_server: {:port, display},
             width: spi_display_opts[:width],
             height: spi_display_opts[:height],
             keyboard_server: get_input_devices(platform)[:keyboard_server]
           }
         }}

      _ ->
        IO.puts("Failed to open display")
        :error
    end
  end
end
