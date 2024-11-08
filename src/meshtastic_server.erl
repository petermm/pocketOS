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

-record(state, {radio, callbacks}).

start_link(Radio, MeshtasticOpts) ->
    gen_server:start_link(?MODULE, [Radio, MeshtasticOpts], []).

handle_payload(Server, {_IfaceId, _Pid} = Iface, Payload, Attributes) ->
    gen_server:call(Server, {handle_payload, Iface, Payload, Attributes}).

init([Radio, MeshtasticOpts]) ->
    Callbacks = proplists:get_value(callbacks, MeshtasticOpts),
    {ok, #state{radio = Radio, callbacks = Callbacks}}.

handle_call({handle_payload, {_IfaceId, _Pid}, Payload, _Attributes}, _From, State) ->
    case meshtastic:parse(Payload) of
        {ok, #{hop_limit := _HopLimit} = Packet} ->
            case meshtastic:decrypt(Packet) of
                #{data := Decrypted} = DecryptedPacket ->
                    Message = meshtastic_proto:decode(Decrypted),
                    DecodedPacket = DecryptedPacket#{message => Message},
                    maybe_callback(State#state.callbacks, message_cb, DecodedPacket),
                    {reply, ok, State};
                Undecryptable ->
                    io:format("Failed meshtastic decrypt: ~p.~n", [Undecryptable]),
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
