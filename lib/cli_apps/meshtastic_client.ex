defmodule CLIApps.MeshtasticClient do
  def start() do
    {:ok, meshtastic_srv} = :meshtastic_server.start_link(self())

    meshtastic_medium_fast_config =
      %{
        tx_power: 14,
        frequency: 869_525_000,
        bandwidth: :bw_250khz,
        bandwidth_hz: 250_000,
        spreading_factor: 9,
        coding_rate: :cr_4_5,
        preamble_length: 16,
        sync_word: 0x2B,
        header_mode: :explicit,
        invert_iq: false,
        enable_crc: false,
        receive_handler: meshtastic_srv
      }

    {:ok, periph_config} = HAL.get_peripheral_config("radio")
    lora_config = Map.merge(meshtastic_medium_fast_config, periph_config)
    {:ok, _lora} = :lora_sx126x.start(lora_config)

    recv_loop()
  end

  defp recv_loop() do
    receive do
      msg ->
        inspect(msg)
        |> :erlang.binary_to_list()
        |> IO.puts()
    end

    recv_loop()
  end
end
