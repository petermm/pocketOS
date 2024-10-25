-module(buttons).

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

-record(state, {listener, button_gpios}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({open, ButtonGPIOs}, _From, _State) ->
    GPIO = gpio:start(),
    maps:foreach(fun(GPIONum, _CodeOrSpecial) ->
            gpio:set_direction(GPIO, GPIONum, input),
            gpio:set_int(GPIO, GPIONum, falling)
        end, ButtonGPIOs),
    {reply, ok, #state{button_gpios = ButtonGPIOs}};

handle_call({subscribe_input, all}, {Pid, _Ref}, State) ->
    {reply, ok, State#state{listener = Pid}};

handle_call(_msg, _from, state) ->
    {reply, error, state}.

handle_cast(_Msg, State) ->
    {reply, error, State}.

handle_info({gpio_interrupt, GPIONum}, #state{listener = Listener, button_gpios = ButtonGPIOs} = State) ->
    #{GPIONum := CodeOrSpecial} = ButtonGPIOs,
    erlang:display({sending_event, Listener, CodeOrSpecial}),
    Listener ! {input_event, self(), erlang:system_time(millisecond), {keyboard, down, CodeOrSpecial}},
    Listener ! {input_event, self(), erlang:system_time(millisecond), {keyboard, up, CodeOrSpecial}},
    {noreply, State};
handle_info(Msg, State) ->
    erlang:display({got, Msg}),
    {noreply, State}.
