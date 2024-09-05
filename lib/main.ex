defmodule Main do
  @display_opts [
    width: 320,
    height: 240
  ]

  def start() do
    :erlang.display("Hello.")

    case :erlang.open_port({:spawn, "display"}, @display_opts) do
      display when is_port(display) ->
        opts = [{:display_server, {:port, display}} | @display_opts]
        {:ok, _ui} = UI.start_link(opts, display_server: {:port, display})

      _ ->
        :io.format("Failed to open display")
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
