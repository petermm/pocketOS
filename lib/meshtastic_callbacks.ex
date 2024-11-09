defmodule MeshtasticCallbacks do
  def init() do
    :micronesia.start()
    :micronesia.create_table(:meshtastic_message)
  end

  def message_cb(%{message: %{portnum: :TEXT_MESSAGE_APP, payload: payload}, packet_id: packet_id}) do
    IO.puts("Got text message: #{inspect(payload)}")
    :micronesia.write({:meshtastic_message, packet_id, payload})
  end

  def message_cb(msg) do
    IO.puts("Got unexpected message: #{inspect(msg)}")
  end
end
