defmodule RadioLauncher do
  def start() do
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
        enable_crc: false
      }

    {:ok, periph_config} = HAL.get_peripheral_config("radio")
    complete_config = Map.merge(meshtastic_medium_fast_config, periph_config)

    {:ok, _rm} =
      :radio_manager.start_link(complete_config, [
        {:meshtastic_server, [callbacks: MeshtasticCallbacks]}
      ])
  end
end
