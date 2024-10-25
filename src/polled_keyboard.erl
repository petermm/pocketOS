-module(polled_keyboard).

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

-record(state, {listener, i2c, address = 16#55}).

-define(GPIO_NUM, 46).

-define(GPIO_KEYB_POWERON, 10).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(open, _From, _State) ->
    GPIO = gpio:start(),

    gpio:set_direction(GPIO, ?GPIO_KEYB_POWERON, output),
    gpio:set_level(GPIO, ?GPIO_KEYB_POWERON, high),

    timer:sleep(500),

    erlang:send_after(500, self(), {gpio_interrupt, ?GPIO_NUM}),

    I2C = i2c:open([{scl_io_num, 8}, {sda_io_num, 18}, {i2c_clock_hz, 100000}]),
    {reply, ok, #state{i2c = I2C}};
handle_call({subscribe_input, all}, {Pid, _Ref}, State) ->
    {reply, ok, State#state{listener = Pid}};
handle_call(_msg, _from, state) ->
    {reply, error, state}.

handle_cast(_Msg, State) ->
    {reply, error, State}.

handle_info(
    {gpio_interrupt, ?GPIO_NUM}, #state{listener = Listener, i2c = I2C, address = Address} = State
) ->
    case i2c:read_bytes(I2C, Address, 1) of
        {ok, <<0>>} ->
            ok;
        {ok, Bin} ->
            erlang:display(Bin),
            C =
                case Bin of
                    <<"\r">> -> $\n;
                    <<ACode/integer>> -> ACode
                end,
            erlang:display(
                {Listener,
                    {input_event, self(), erlang:system_time(millisecond), {keyboard, down, C}}}
            ),
            Listener ! {input_event, self(), erlang:system_time(millisecond), {keyboard, down, C}},
            Listener ! {input_event, self(), erlang:system_time(millisecond), {keyboard, up, C}};
        NotOk ->
            erlang:display({not_ok, NotOk})
    end,
    erlang:send_after(500, self(), {gpio_interrupt, ?GPIO_NUM}),
    {noreply, State};
handle_info(Msg, State) ->
    erlang:display({got, Msg}),
    {noreply, State}.
