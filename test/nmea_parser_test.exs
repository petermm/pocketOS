defmodule NMEAParserTest do
  use ExUnit.Case

  test "tokenize single nmea sentence" do
    assert :nmea_parser.tokenize_next(
             <<"$GPGLL,4524.46341,N,01153.19655,E,175845.00,A,A*68\r\n">>,
             []
           ) == {:ok, ["GPGLL", "4524.46341", "N", "01153.19655", "E", "175845.00", "A", "A"], ""}

    assert :nmea_parser.tokenize_next(
             "$GPGGA,202530.00,4532.47819,N,00724.11583,E,5,40,0.5,1097.36,M,-17.00,M,18,TSTR*7A\r\n",
             []
           ) ==
             {:ok,
              [
                "GPGGA",
                "202530.00",
                "4532.47819",
                "N",
                "00724.11583",
                "E",
                "5",
                "40",
                "0.5",
                "1097.36",
                "M",
                "-17.00",
                "M",
                "18",
                "TSTR"
              ], ""}
  end

  test "tokenize two nmea sentences" do
    assert :nmea_parser.tokenize_next(
             "$GPGLL,3538.86108,N,13938.82696,E,202725.00,A,D*65\r\n$GPGLL,5439.47207,S,06507.12794,W,202725.00,A,D*63\r\n",
             []
           ) ==
             {:ok, ["GPGLL", "3538.86108", "N", "13938.82696", "E", "202725.00", "A", "D"],
              "$GPGLL,5439.47207,S,06507.12794,W,202725.00,A,D*63\r\n"}
  end

  test "tokenize reject invalid checksum" do
    assert :nmea_parser.tokenize_next(
             <<"$GPGLL,4532.47819,N,00724.11583,E,175845.00,A,A*65\r\n">>,
             []
           ) == {:error, :checksum_mismatch, ""}
  end

  test "tokenize recovers after checksum error" do
    assert :nmea_parser.tokenize_next(
             <<"$GPGLL,4524.46341,N,01153.19655,E,175845.00,A,A*31\r\n$GPGLL,5439.47207,S,06507.12794,W,175845.00,A,A*6C\r\n">>,
             []
           ) ==
             {:error, :checksum_mismatch,
              "$GPGLL,5439.47207,S,06507.12794,W,175845.00,A,A*6C\r\n"}
  end

  test "tokenize recover after truncated statement" do
    assert :nmea_parser.tokenize_next(
             <<"90,36,07,67,279,42,12,29,323,36*77\r\n$GPGLL,4532.47819,N,00724.11583,E,195144.00,A,A*69\r\n">>,
             []
           ) == {:ok, ["GPGLL", "4532.47819", "N", "00724.11583", "E", "195144.00", "A", "A"], ""}
  end

  test "tokenize ignores garbage" do
    assert :nmea_parser.tokenize_next(
             <<"\nqweweqwq\r\n\r\r\nerewrwrwerrwewer\n">>,
             []
           ) == {:cont, "", []}
  end

  test "tokenize and parse can be used together" do
    {:ok, tokens, ""} =
      :nmea_parser.tokenize_next("$GPGLL,4524.46341,N,01153.19655,E,232726.00,A,D*67\r\n", [])

    assert :nmea_parser.parse(tokens) ==
             {:ok,
              %{
                sentence: :gll,
                talker: :gps,
                lat: {45, 24.46341, :n},
                lon: {11, 53.19655, :e},
                utc_time: {23, 27, 26.00},
                data_status: :valid,
                mode_ind: :differential
              }}
  end

  test "parse a GPGGA sentence" do
    assert :nmea_parser.parse([
             "GPGGA",
             "012411.00",
             "4532.47819",
             "N",
             "00724.11583",
             "E",
             "5",
             "40",
             "0.5",
             "1097.36",
             "M",
             "-17.00",
             "M",
             "18",
             "TSTR"
           ]) ==
             {:ok,
              %{
                sentence: :gga,
                talker: :gps,
                utc_time: {1, 24, 11.00},
                lat: {45, 32.47819, :n},
                lon: {7, 24.11583, :e},
                quality: 5,
                n_sats: 40,
                hdop: 0.5,
                alt: 1097.36,
                undulation: -17.0,
                age: 18,
                station_id: "TSTR"
              }}
  end

  test "parse a GPGLL sentence" do
    assert :nmea_parser.parse([
             "GPGLL",
             "3538.86108",
             "N",
             "13938.82696",
             "E",
             "134748.00",
             "A",
             "D"
           ]) ==
             {:ok,
              %{
                sentence: :gll,
                talker: :gps,
                lat: {35, 38.86108, :n},
                lon: {139, 38.82696, :e},
                utc_time: {13, 47, 48.00},
                data_status: :valid,
                mode_ind: :differential
              }}
  end

  test "parse a GNGLL sentence" do
    assert :nmea_parser.parse([
             "GNGLL",
             "5439.47207",
             "S",
             "06507.12794",
             "W",
             "174739.05",
             "A",
             "D"
           ]) ==
             {:ok,
              %{
                sentence: :gll,
                talker: :gnss,
                lat: {54, 39.47207, :s},
                lon: {65, 7.12794, :w},
                utc_time: {17, 47, 39.05},
                data_status: :valid,
                mode_ind: :differential
              }}
  end

  test "parse a GPGSV sentence" do
    assert :nmea_parser.parse([
             "GPGSV",
             "4",
             "1",
             "16",
             "02",
             "82",
             "150",
             "53",
             "11",
             "78",
             "139",
             "",
             "12",
             "72",
             "191",
             "53",
             "25",
             "50",
             "296",
             "51"
           ]) ==
             {:ok,
              %{
                sentence: :gsv,
                talker: :gps,
                n_msgs: 4,
                msg_n: 1,
                n_sats: 16,
                sats: [
                  %{prn: 2, elevation: 82, azimuth: 150, snr: 53},
                  %{prn: 11, elevation: 78, azimuth: 139},
                  %{prn: 12, elevation: 72, azimuth: 191, snr: 53},
                  %{prn: 25, elevation: 50, azimuth: 296, snr: 51}
                ]
              }}
  end

  ## da fare pollaio
  test "parse a GPRMC sentence" do
    assert :nmea_parser.parse([
             "GPRMC",
             "000122.00",
             "A",
             "4524.46341",
             "N",
             "01153.19655",
             "E",
             "0.004",
             "133.4",
             "010224",
             "0.0",
             "E",
             "D"
           ]) ==
             {:ok,
              %{
                sentence: :rmc,
                talker: :gps,
                utc_time: {0, 1, 22.00},
                data_status: :valid,
                lat: {45, 24.46341, :n},
                lon: {11, 53.19655, :e},
                speed_over_ground_kn: 0.004,
                track_made_good_deg: 133.4,
                date: {2024, 2, 1},
                magnetic_variation_deg: {0.0, :e},
                mode_ind: :differential
              }}
  end

  test "parse a GPZDA sentence" do
    assert :nmea_parser.parse(["GPZDA", "204007.00", "13", "05", "2022", "", ""]) ==
             {:ok,
              %{
                sentence: :zda,
                talker: :gps,
                utc_time: {20, 40, 07.00},
                date: {2022, 5, 13},
                utc_offset: {:undefined, :undefined}
              }}
  end

  test "parse a NMEA 4.10 GNRMC sentence" do
    assert :nmea_parser.parse([
             "GNRMC",
             "163445.00",
             "A",
             "4532.47819",
             "N",
             "00724.11583",
             "E",
             "0.153",
             "",
             "191124",
             "",
             "",
             "A",
             "V"
           ]) ==
             {:ok,
              %{
                sentence: :rmc,
                talker: :gnss,
                utc_time: {16, 34, 45.00},
                data_status: :valid,
                lat: {45, 32.47819, :n},
                lon: {7, 24.11583, :e},
                speed_over_ground_kn: 0.153,
                date: {2024, 11, 19},
                mode_ind: :autonomous,
                nav_status: :valid
              }}
  end
end
