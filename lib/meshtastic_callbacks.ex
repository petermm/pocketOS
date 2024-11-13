defmodule MeshtasticCallbacks do
  def init() do
    :micronesia.start()
    :micronesia.create_table(:meshtastic_message)
  end

  def message_cb(%{message: %{portnum: :TEXT_MESSAGE_APP, payload: payload}, packet_id: packet_id}) do
    IO.puts("Got text message: #{inspect(payload)}")
    :micronesia.dirty_write({:meshtastic_message, packet_id, payload})
  end

  def message_cb(msg) do
    IO.puts("Got unexpected message: #{inspect(msg)}")
  end

  def send_text_message(text) do
    data =
      %{portnum: :TEXT_MESSAGE_APP, payload: text}
      |> :meshtastic_proto.encode()
      |> :erlang.iolist_to_binary()

    :ok = :meshtastic_server.send(:meshtastic_server, 0xFFFFFFFF, data)
  end
end
