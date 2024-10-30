-module(meshtastic_server).

-behavior(gen_server).

-export([
    start_link/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {radio}).

start_link(Radio) ->
    gen_server:start_link(?MODULE, [Radio], []).

init(Radio) ->
    {ok, #state{radio = Radio}}.

handle_call(_msg, _from, state) ->
    {reply, error, state}.

handle_cast(_Msg, State) ->
    {reply, error, State}.

handle_info({lora_receive, {RModel, RadioPid}, Payload, #{snr := Snr, rssi := Rssi}} = T, State) ->
    erlang:display({recv, T}),
    case meshtastic:parse(Payload) of
        {ok, #{hop_limit := HopLimit} = Packet} ->
            case meshtastic:decrypt(Packet) of
                #{data := Decrypted} = DecryptedPacket ->
                    Message = meshtastic_proto:decode(Decrypted),
                    DecodedPacket = DecryptedPacket#{message => Message},
                    io:format("Message ~p~n", [DecodedPacket])
            end;
        SomethingElse ->
            io:format("Unexpected ~p~n", [SomethingElse])
    end,
    {noreply, State};
handle_info(Msg, State) ->
    io:format("radio_got: ~p~n", [Msg]),
    {noreply, State}.
