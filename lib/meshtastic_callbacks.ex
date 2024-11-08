defmodule MeshtasticCallbacks do
  def message_cb(%{message: %{portnum: :TEXT_MESSAGE_APP, payload: payload}}) do
    IO.puts("Got text message: #{inspect(payload)}")
  end

  def message_cb(msg) do
    IO.puts("Got unexpected message: #{inspect(msg)}")
  end
end
