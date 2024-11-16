-module(nmea_parser).
-export([tokenize_next/2, parse/1]).

%% TODO: add more validity constraints, such as the 46th of January is not a valid date

tokenize_next(<<$$, Rest/binary>>, PartialTokens) ->
    tokens(Rest, PartialTokens);
tokenize_next(<<_Discard, Rest/binary>>, PartialTokens) ->
    tokenize_next(Rest, PartialTokens);
tokenize_next(<<>>, PartialTokens) ->
    tokens(<<>>, PartialTokens).

tokens(In, Tokens) when is_binary(In) ->
    tokens(In, 0, 0, Tokens).

tokens(In, _Len, TotalLen, _Tokens) when TotalLen > 82 ->
    {error, malformed, In};
tokens(In, Len, TotalLen, Tokens) when is_binary(In) and is_integer(Len) ->
    case In of
        <<_S:Len/binary, $$, _NextStatement/binary>> ->
            <<_:Len/binary, Rest/binary>> = In,
            {error, malformed, Rest};
        <<S:Len/binary, $,, Rest/binary>> ->
            tokens(Rest, 0, TotalLen + 1, [S | Tokens]);
        <<S:Len/binary, $*, Rest/binary>> ->
            tokens(Rest, 0, TotalLen + 1, [checksum, S | Tokens]);
        <<S:Len/binary, "\r\n", Rest/binary>> ->
            terminate_tokenize([S | Tokens], Rest);
        <<S:Len/binary, "\r\n">> ->
            terminate_tokenize([S | Tokens], <<"">>);
        <<_S:Len/binary, _Rest/binary>> ->
            tokens(In, Len + 1, TotalLen + 1, Tokens);
        Truncated ->
            {cont, Truncated, Tokens}
    end.

terminate_tokenize([Checksum, checksum | Tail], Rest) ->
    TokensList = lists:reverse(Tail),
    case verify_list(TokensList, Checksum) of
        true -> {ok, TokensList, Rest};
        false -> {error, checksum_mismatch, Rest}
    end;
terminate_tokenize(_Unexpected, Rest) ->
    {error, malformed, Rest}.

verify_list([_Head | _Tail] = Tokens, ExpectedAsHexString) ->
    verify_list(Tokens, 0, ExpectedAsHexString);
verify_list(_Tokens, _ExpectedAsHexString) ->
    false.

verify_list([H], Checksum, ExpectedAsHexString) when is_binary(H) ->
    Calculated = checksum_bin(H, Checksum),
    hex_to_int(ExpectedAsHexString) == Calculated;
verify_list([H | T], Checksum, ExpectedAsHexString) when is_binary(H) ->
    verify_list(T, checksum_bin(H, Checksum) bxor $,, ExpectedAsHexString);
verify_list(_InvalidTokens, _Checksum, _ExpectedAsHexString) ->
    false.

hex_to_int(<<Digit0, Digit1>>) ->
    case hex_digit_to_int(Digit0) of
        D0 when is_integer(D0) ->
            case hex_digit_to_int(Digit1) of
                D1 when is_integer(D1) -> (D0 bsl 4) bor D1;
                _ -> invalid
            end;
        _ ->
            invalid
    end.

hex_digit_to_int(HD) ->
    case HD of
        Num when Num >= $0 andalso Num =< $9 -> Num - $0;
        Alpha when Alpha >= $A andalso Alpha =< $F -> Alpha - $A + 10;
        _ -> invalid
    end.

checksum_bin(<<"">>, Checksum) ->
    Checksum;
checksum_bin(<<C, Rest/binary>>, Checksum) ->
    checksum_bin(Rest, C bxor Checksum).

parse(
    [<<TalkerBin:2/binary, "GLL">>, LatStr, NSStr, LonStr, EWStr, Time, DataStatusBin, ModeIndBin] =
        Sentence
) ->
    try
        Talker = id_to_talker(TalkerBin),
        {ok, Lat} = parse_lat(LatStr, NSStr),
        {ok, Lon} = parse_lon(LonStr, EWStr),
        {ok, TimeTuple} = parse_time(Time),
        {ok, DataStatus} = parse_data_status(DataStatusBin),
        {ok, ModeInd} = parse_mode_ind(ModeIndBin),
        #{
            sentence => gll,
            talker => Talker,
            lat => Lat,
            lon => Lon,
            utc_time => TimeTuple,
            data_status => DataStatus,
            mode_ind => ModeInd
        }
    of
        Valid -> {ok, Valid}
    catch
        error:{badmatch, {error, invalid}} -> {skip, Sentence};
        error:badarg -> {skip, Sentence}
    end;
parse(
    [
        <<TalkerBin:2/binary, "GGA">>,
        Time,
        LatStr,
        NSStr,
        LonStr,
        EWStr,
        FixQualityStr,
        NumSatsStr,
        HDoPStr,
        AltStr,
        <<"M">>,
        UndStr,
        <<"M">>,
        AgeStr,
        StnId
    ] = Sentence
) ->
    try
        Talker = id_to_talker(TalkerBin),
        {ok, Lat} = parse_lat(LatStr, NSStr),
        {ok, Lon} = parse_lon(LonStr, EWStr),
        {ok, TimeTuple} = parse_time(Time),
        FixQuality = binary_to_integer(FixQualityStr),
        NumSats = binary_to_integer(NumSatsStr),
        HDoP = binary_to_float(HDoPStr),
        Alt = binary_to_float(AltStr),
        Und = binary_to_float(UndStr),
        Age = binary_to_integer(AgeStr),
        #{
            sentence => gga,
            talker => Talker,
            utc_time => TimeTuple,
            lat => Lat,
            lon => Lon,
            quality => FixQuality,
            n_sats => NumSats,
            hdop => HDoP,
            alt => Alt,
            undulation => Und,
            age => Age,
            station_id => StnId
        }
    of
        Valid -> {ok, Valid}
    catch
        error:{badmatch, {error, invalid}} -> {skip, Sentence};
        error:badarg -> {skip, Sentence}
    end;
parse([<<TalkerBin:2/binary, "GSV">>, NMsgBin, MsgNBin, NSatsBin | SatList] = Sentence) ->
    try
        Talker = id_to_talker(TalkerBin),
        NMsg = binary_to_integer(NMsgBin),
        MsgN = binary_to_integer(MsgNBin),
        NSats = binary_to_integer(NSatsBin),

        #{
            sentence => gsv,
            talker => Talker,
            n_msgs => NMsg,
            msg_n => MsgN,
            n_sats => NSats,
            sats => parse_sat_list(SatList, [])
        }
    of
        Valid -> {ok, Valid}
    catch
        error:{badmatch, {error, invalid}} -> {skip, Sentence};
        error:badarg -> {skip, Sentence}
    end;
parse(
    [
        <<TalkerBin:2/binary, "RMC">>,
        UTCBin,
        DataStatusBin,
        LatBin,
        NSBin,
        LonBin,
        EWBin,
        SpeedKnBin,
        TrackMadeGoodDegBin,
        DateBin,
        MagVarBin,
        MagEWBin,
        ModeIndBin
        | Optional
    ] = Sentence
) ->
    try
        Talker = id_to_talker(TalkerBin),
        {ok, TimeTuple} = parse_time(UTCBin),
        {ok, DataStatus} = parse_data_status(DataStatusBin),
        {ok, Lat} = parse_lat(LatBin, NSBin),
        {ok, Lon} = parse_lon(LonBin, EWBin),
        SpeedKn = binary_to_float(SpeedKnBin),
        TrackMadeGoodDeg = parse_optional_float(TrackMadeGoodDegBin),
        {ok, Date} = parse_date(DateBin),
        MagVarDeg =
            case MagVarBin of
                <<"">> ->
                    undefined;
                NotEmptyMagVar when is_binary(NotEmptyMagVar) ->
                    {ok, MagEW} = parse_ew(MagEWBin),
                    {binary_to_float(NotEmptyMagVar), MagEW}
            end,
        {ok, ModeInd} = parse_mode_ind(ModeIndBin),
        NavStatus =
            case Optional of
                [] -> undefined;
                [NavStatusBin] -> parse_nav_status(NavStatusBin)
            end,

        RMC_0 =
            #{
                sentence => rmc,
                talker => Talker,
                utc_time => TimeTuple,
                data_status => DataStatus,
                lat => Lat,
                lon => Lon,
                speed_over_ground_kn => SpeedKn,
                date => Date,
                mode_ind => ModeInd
            },
        RMC_1 = maybe_put(track_made_good_deg, TrackMadeGoodDeg, RMC_0),
        RMC_2 = maybe_put(magnetic_variation_deg, MagVarDeg, RMC_1),
        maybe_put(nav_status, NavStatus, RMC_2)
    of
        Valid -> {ok, Valid}
    catch
        error:{badmatch, {error, invalid}} -> {skip, Sentence};
        error:badarg -> {skip_failed, Sentence}
    end;
parse(
    [<<TalkerBin:2/binary, "ZDA">>, UTCBin, DDBin, MMBin, YYYYBin, UTCHHOffBin, UTCMMOffBin] =
        Sentence
) ->
    try
        Talker = id_to_talker(TalkerBin),
        {ok, TimeTuple} = parse_time(UTCBin),
        DD = binary_to_integer(DDBin),
        MM = binary_to_integer(MMBin),
        YYYY = binary_to_integer(YYYYBin),
        UTCHHOff = parse_optional_integer(UTCHHOffBin),
        UTCMMOff = parse_optional_integer(UTCMMOffBin),
        #{
            sentence => zda,
            talker => Talker,
            utc_time => TimeTuple,
            date => {YYYY, MM, DD},
            utc_offset => {UTCHHOff, UTCMMOff}
        }
    of
        Valid -> {ok, Valid}
    catch
        error:{badmatch, {error, invalid}} -> {skip, Sentence};
        error:badarg -> {skip, Sentence}
    end;
parse(Any) ->
    {skip, Any}.

maybe_put(_Key, undefined, Map) ->
    Map;
maybe_put(Key, Value, Map) ->
    Map#{Key => Value}.

id_to_talker(Id) ->
    % TODO: not clear if beidou is GB or BD
    case Id of
        <<"GA">> -> galileo;
        <<"GB">> -> beidou;
        <<"BD">> -> beidou;
        <<"GI">> -> navic;
        <<"GL">> -> glonass;
        <<"GN">> -> gnss;
        <<"GP">> -> gps;
        <<"GQ">> -> qzss;
        _ -> error(badarg)
    end.

parse_sat_list([], Acc) ->
    lists:reverse(Acc);
parse_sat_list([PRNBin, ElevBin, AzimuthBin, SNRBin | Rest], Acc) ->
    PRN = binary_to_integer(PRNBin),
    Elev = binary_to_integer(ElevBin),
    Azimuth = binary_to_integer(AzimuthBin),
    SNR = parse_optional_integer(SNRBin),

    Sat_0 = #{prn => PRN, elevation => Elev, azimuth => Azimuth},
    Sat = maybe_put(snr, SNR, Sat_0),

    parse_sat_list(Rest, [Sat | Acc]).

parse_optional_float(OptIntBin) ->
    case OptIntBin of
        <<"">> -> undefined;
        NotNull when is_binary(NotNull) -> binary_to_float(NotNull)
    end.

parse_optional_integer(OptIntBin) ->
    case OptIntBin of
        <<"">> -> undefined;
        NotNull when is_binary(NotNull) -> binary_to_integer(NotNull)
    end.

parse_lat(LatBin, NSBin) ->
    case parse_coord(LatBin) of
        {ok, {Lat1, Lat2}} ->
            case parse_ns(NSBin) of
                {ok, NS} -> {ok, {Lat1, Lat2, NS}};
                _ -> {error, invalid}
            end;
        _ ->
            {error, invalid}
    end.

parse_lon(LonBin, EWBin) ->
    case parse_coord(LonBin) of
        {ok, {Lon1, Lon2}} ->
            case parse_ew(EWBin) of
                {ok, EW} -> {ok, {Lon1, Lon2, EW}};
                _ -> {error, invalid}
            end;
        _ ->
            {error, invalid}
    end.

parse_coord(In) ->
    parse_coord(In, 1).

parse_coord(In, Len) when is_binary(In) and is_integer(Len) ->
    case In of
        <<_S1:Len/binary, _S2:2/binary, $., _Rest/binary>> ->
            <<Deg:Len/binary, Min/binary>> = In,
            try {ok, {binary_to_integer(Deg), binary_to_float(Min)}} of
                Valid -> Valid
            catch
                error:badarg ->
                    {error, invalid_coord}
            end;
        <<_S:Len/binary, _Rest/binary>> ->
            parse_coord(In, Len + 1);
        _Invalid ->
            {error, invalid_coord}
    end.

parse_time(<<HH:2/binary, MM:2/binary, SS_SS:5/binary>>) ->
    try {binary_to_integer(HH), binary_to_integer(MM), binary_to_float(SS_SS)} of
        Valid -> {ok, Valid}
    catch
        error:badarg -> {error, invalid}
    end;
parse_time(Invalid) when is_binary(Invalid) ->
    {error, invalid}.

parse_date(<<DD:2/binary, MM:2/binary, YY:2/binary>>) ->
    % Update this code on year 2100
    try {2000 + binary_to_integer(YY), binary_to_integer(MM), binary_to_integer(DD)} of
        Valid -> {ok, Valid}
    catch
        error:badarg -> {error, invalid}
    end;
parse_date(Invalid) when is_binary(Invalid) ->
    {error, invalid}.

parse_ns(NS) ->
    case NS of
        <<"N">> -> {ok, n};
        <<"S">> -> {ok, s};
        _ -> {error, invalid}
    end.

parse_ew(EW) ->
    case EW of
        <<"E">> -> {ok, e};
        <<"W">> -> {ok, w};
        _ -> {error, invalid}
    end.

parse_data_status(AV) ->
    case AV of
        <<"A">> -> {ok, valid};
        <<"V">> -> {ok, invalid}
    end.

parse_mode_ind(DS) ->
    case DS of
        <<"A">> -> {ok, autonomous};
        <<"D">> -> {ok, differential};
        <<"E">> -> {ok, estimated};
        <<"M">> -> {ok, manual};
        <<"N">> -> {ok, invalid}
    end.

parse_nav_status(DS) ->
    case DS of
        <<"A">> -> autonomous;
        <<"D">> -> differential;
        <<"E">> -> estimated;
        <<"M">> -> manual;
        <<"N">> -> invalid;
        <<"S">> -> simulator;
        <<"V">> -> valid;
        Other when is_binary(Other) -> error(badarg)
    end.
