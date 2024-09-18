-module(face).

-behavior(gen_server).

-export([
    start_link/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {listener, i2c}).

-define(GPIO_NUM, 5).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    erlang:display("Init face genserver"),
    {ok, #state{}}.

handle_call(open, _From, _State) ->
    GPIO = gpio:open(),
    gpio:set_direction(GPIO, 5, input),
    gpio:set_int(GPIO, 5, falling),
    I2C = i2c:open([{scl_io_num, 22}, {sda_io_num, 21}, {i2c_clock_hz, 100000}]),
    {reply, ok, #state{i2c = I2C}};

handle_call({subscribe_input, all}, {Pid, _Ref}, State) ->
    {reply, ok, State#state{listener = Pid}};

handle_call(_msg, _from, state) ->
    {reply, error, state}.

handle_cast(_Msg, State) ->
    {reply, error, State}.

handle_info({gpio_interrupt, ?GPIO_NUM}, #state{listener = Listener, i2c = I2C} = State) ->
    case i2c:read_bytes(I2C, 16#08, 1) of
        {ok, Bin} ->
            erlang:display(Bin),
            C = case Bin of
                <<"\r">> -> $\n;
                <<ACode/integer>> -> ACode
            end,
            erlang:display({Listener, {input_event, self(), erlang:system_time(millisecond), {keyboard, down, C}}}),
            Listener ! {input_event, self(), erlang:system_time(millisecond), {keyboard, down, C}},
            Listener ! {input_event, self(), erlang:system_time(millisecond), {keyboard, up, C}};
        NotOk ->
            erlang:display({not_ok, NotOk})
    end,
    {noreply, State};
handle_info(Msg, State) ->
    erlang:display({got, Msg}),
    {noreply, State}.
