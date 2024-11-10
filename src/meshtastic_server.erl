-module(meshtastic_server).

-behavior(gen_server).

-export([
    start_link/2,
    handle_payload/4
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {radio, callbacks, last_seen = #{}}).

-define(PACKET_SEEN_EXPIRY_SEC, 30).

start_link(Radio, MeshtasticOpts) ->
    gen_server:start_link(?MODULE, [Radio, MeshtasticOpts], []).

handle_payload(Server, {_IfaceId, _Pid} = Iface, Payload, Attributes) ->
    gen_server:call(Server, {handle_payload, Iface, Payload, Attributes}).

init([Radio, MeshtasticOpts]) ->
    Callbacks = proplists:get_value(callbacks, MeshtasticOpts),
    {ok, #state{radio = Radio, callbacks = Callbacks}}.

handle_call({handle_payload, {_IfaceId, _Pid}, Payload, _Attributes}, _From, State) ->
    case meshtastic:parse(Payload) of
        {ok, #{hop_limit := _HopLimit, src := Src, packet_id := PacketId} = Packet} ->
            MonotonicTS = erlang:monotonic_time(second),
            {Duplicated, UpdatedLastSeen} = update_last_seen(
                State#state.last_seen, Src, PacketId, MonotonicTS
            ),
            PrunedLastSeen = prune_expired_last_seen(UpdatedLastSeen, MonotonicTS),
            if
                not Duplicated ->
                    case meshtastic:decrypt(Packet) of
                        #{data := Decrypted} = DecryptedPacket ->
                            Message = meshtastic_proto:decode(Decrypted),
                            DecodedPacket = DecryptedPacket#{message => Message},
                            maybe_callback(State#state.callbacks, message_cb, DecodedPacket),
                            {reply, ok, State#state{last_seen = PrunedLastSeen}};
                        Undecryptable ->
                            % We don't update last seen when we receive a corrupt packet
                            % just in case next retransmision is ok
                            io:format("Failed meshtastic decrypt: ~p.~n", [Undecryptable]),
                            {reply, discard, State}
                    end;
                true ->
                    io:format("Discarding duplicated: ~p.~n", [PacketId]),
                    {reply, discard, State}
            end;
        _SomethingElse ->
            {reply, next, State}
    end;
handle_call(_msg, _from, State) ->
    {reply, error, State}.

handle_cast(_msg, State) ->
    {reply, error, State}.

handle_info(_msg, State) ->
    {noreply, State}.

maybe_callback(undefined, _, _) ->
    ok;
maybe_callback(Mod, Callback, Arg) ->
    Mod:Callback(Arg).

update_last_seen(LastSeenMap, Source, PacketId, MonotonicSec) ->
    {AlreadySeen, UpdatedLastSeenMap} =
        case LastSeenMap of
            #{Source := #{PacketId := LastTimestamp} = SeenPacketMap} ->
                if
                    MonotonicSec > LastTimestamp ->
                        {true, LastSeenMap#{Source => SeenPacketMap#{PacketId => MonotonicSec}}};
                    true ->
                        {true, LastSeenMap}
                end;
            #{Source := SeenPacketMap} ->
                {false, LastSeenMap#{Source => SeenPacketMap#{PacketId => MonotonicSec}}};
            _ ->
                {false, LastSeenMap#{Source => #{PacketId => MonotonicSec}}}
        end.

prune_expired_last_seen(LastSeenMap, MonotonicSec) ->
    PrunedMap =
        maps:map(
            fun(_Source, SeenPacketMap) ->
                maps:filter(
                    fun(PacketId, LastTimestamp) ->
                        if
                            LastTimestamp + ?PACKET_SEEN_EXPIRY_SEC < MonotonicSec ->
                                false;
                            true ->
                                true
                        end
                    end,
                    SeenPacketMap
                )
            end,
            LastSeenMap
        ),
    maps:filter(fun(_Source, SeenPacketMap) -> SeenPacketMap =/= #{} end, PrunedMap).
