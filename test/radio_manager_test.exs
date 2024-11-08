defmodule RadioManagerTest do
  use ExUnit.Case
  doctest PocketOS

  defmodule MockRadio do
    def start(%{receive_handler: receive_handler}) when is_pid(receive_handler) do
      {:ok,
       spawn(fn ->
         iface = {MockRadio, self()}

         payload_0 = <<0>>

         send(receive_handler, {:lora_receive, iface, payload_0, %{rssi: -28, snr: 11}})

         payload_1 = <<1>>

         send(receive_handler, {:lora_receive, iface, payload_1, %{rssi: -28, snr: 11}})
       end)}
    end
  end

  defmodule MockProtocolHandler do
    def start_link(_, _) do
      {:ok, :server}
    end

    def handle_payload(:server, {MockRadio, _pid} = _iface, payload, %{} = _attributes) do
      case payload do
        <<0>> ->
          send(:radio_manager_tester, :tested)
          :ok

        <<1>> ->
          send(:radio_manager_tester, :tested)
          :next
      end
    end
  end

  test "radio manager works" do
    Process.register(self(), :radio_manager_tester)

    radio_config = %{
      radio_module: MockRadio
    }

    {:ok, _server} = :radio_manager.start_link(radio_config, [{MockProtocolHandler, []}])

    assert_receive(:tested, 5000)
    assert_receive(:tested, 5000)
  end
end
