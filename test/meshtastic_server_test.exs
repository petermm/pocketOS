defmodule MeshtasticServerTest do
  use ExUnit.Case
  doctest PocketOS

  defmodule TestCallbacks do
    def message_cb(msg) do
      send(:meshtastic_server_tester, msg)
    end
  end

  defmodule TestRadio do
    def broadcast(:mock, payload) do
      :io.format("Going to send ~p~n", [payload])
      :ok
    end
  end

  test "handle a text message" do
    Process.register(self(), :meshtastic_server_tester)

    {:ok, server} =
      :meshtastic_server.start_link({TestRadio, TestRadio, :mock}, callbacks: TestCallbacks)

    iface = {:mock, :undefined}

    payload_0 =
      <<255, 255, 255, 255, 132, 70, 49, 67, 214, 65, 11, 101, 99, 31, 0, 0, 128, 234, 186, 192,
        51, 252, 136, 181, 75, 187, 247, 119, 124, 9, 148, 58, 83, 188, 81, 148>>

    :ok = :meshtastic_server.handle_payload(server, iface, payload_0, %{rssi: -28, snr: 11})

    assert_receive(
      %{
        want_ack: false,
        dest: 4_294_967_295,
        message: %{portnum: :TEXT_MESSAGE_APP, payload: <<"ciaone a tutti">>}
      },
      5000
    )

    payload_1 = <<0>>
    :next = :meshtastic_server.handle_payload(server, iface, payload_1, %{rssi: -20, snr: 15})

    :ok = :gen_server.stop(server)
    Process.unregister(:meshtastic_server_tester)
  end

  test "duplicates are discarded" do
    Process.register(self(), :meshtastic_server_tester)

    {:ok, server} =
      :meshtastic_server.start_link({TestRadio, TestRadio, :mock}, callbacks: TestCallbacks)

    iface = {:mock, :undefined}

    payload_0 =
      <<255, 255, 255, 255, 132, 70, 49, 67, 214, 65, 11, 101, 99, 31, 0, 0, 128, 234, 186, 192,
        51, 252, 136, 181, 75, 187, 247, 119, 124, 9, 148, 58, 83, 188, 81, 148>>

    :ok = :meshtastic_server.handle_payload(server, iface, payload_0, %{rssi: -28, snr: 11})
    :discard = :meshtastic_server.handle_payload(server, iface, payload_0, %{rssi: -10, snr: 12})

    :ok = :gen_server.stop(server)
    Process.unregister(:meshtastic_server_tester)
  end
end
