defmodule Main do
  def start() do
    :erlang.display("Hello.")

    with {:ok, initialized} <- HAL.init(),
         %{display: initialized_display} <- initialized,
         %{display_server: display_server, width: width, height: height} <- initialized_display do
      opts = [
        width: width,
        height: height,
        display_server: display_server,
        keyboard_server: initialized_display[:keyboard_server]
      ]

      {:ok, _ui} = UI.start_link(opts, [display_server: display_server] ++ opts)

      if HAL.has_peripheral?("radio") do
        RadioLauncher.start()
      else
        :ok
      end
    else
      _ ->
        IO.puts("Failed HAL init.")
    end

    recv_loop()
  end

  defp recv_loop() do
    receive do
      any -> :erlang.display({:got, any})
    end

    recv_loop()
  end
end
