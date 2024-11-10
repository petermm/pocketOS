-module(micronesia).

-behavior(gen_server).

-export([
    start/0,
    create_table/1,
    delete_table/1,
    all/1,
    dirty_read/1,
    dirty_write/1,
    subscribe/1,
    unsubscribe/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {tables = #{}, table_subscribers = #{}}).

start() ->
    case gen_server:start({local, micronesia}, ?MODULE, [], []) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.

create_table(Name) ->
    gen_server:call(micronesia, {create_table, Name}).

delete_table(Name) ->
    gen_server:call(micronesia, {delete_table, Name}).

all(Name) ->
    gen_server:call(micronesia, {all, Name}).

dirty_read({_Name, _Id} = T) ->
    gen_server:call(micronesia, {dirty_read, T}).

dirty_write(Record) ->
    gen_server:call(micronesia, {dirty_write, Record}).

subscribe({table, Tab, simple}) ->
    gen_server:call(micronesia, {subscribe, table, Tab, simple}).

unsubscribe({table, Tab, simple}) ->
    gen_server:call(micronesia, {unsubscribe, table, Tab, simple}).

init([]) ->
    {ok, #state{}}.

handle_call({create_table, Name}, _From, State) ->
    Tables = State#state.tables,
    WithNameTable = Tables#{Name => []},
    {reply, {atomic, ok}, State#state{tables = WithNameTable}};
handle_call({delete_table, Name}, _From, State) ->
    Tables = State#state.tables,
    WithoutTable = maps:remove(Name, Tables),
    WithoutSubscribers = remove_table_subscribers(State#state.table_subscribers, Name),
    {reply, {atomic, ok}, State#state{
        tables = WithoutTable, table_subscribers = WithoutSubscribers
    }};
handle_call({all, TableName}, _From, State) ->
    Tables = State#state.tables,
    case Tables of
        #{TableName := Table} ->
            Records =
                lists:map(
                    fun(Row) ->
                        erlang:insert_element(1, Row, TableName)
                    end,
                    Table
                ),
            {reply, Records, State};
        _ ->
            {reply, error, State}
    end;
handle_call({dirty_read, {TableName, Id}}, _From, State) ->
    Tables = State#state.tables,
    case Tables of
        #{TableName := Table} ->
            case lists:keyfind(Id, 1, Table) of
                false ->
                    {reply, [], State};
                Value when is_tuple(Value) ->
                    Result = erlang:insert_element(1, Value, TableName),
                    {reply, [Result], State}
            end;
        _ ->
            {reply, error, State}
    end;
handle_call({dirty_write, Record}, {Client, _Tag}, State) ->
    TableName = element(1, Record),
    RecordId = element(2, Record),
    Columns = erlang:delete_element(1, Record),
    Tables = State#state.tables,
    case Tables of
        #{TableName := Table} ->
            NewTable = lists:keystore(RecordId, 1, Table, Columns),
            Event = {mnesia_table_event, {write, Record, {dirty, Client}}},
            maybe_send_event(State#state.table_subscribers, TableName, Event),
            {reply, ok, State#state{tables = Tables#{TableName := NewTable}}};
        _ ->
            {reply, error, State}
    end;
handle_call({subscribe, table, TableName, simple}, {Client, _Tag}, State) ->
    TS = State#state.table_subscribers,
    {Result, UpdatedTabSubs} =
        case TS of
            #{TableName := Table} ->
                case lists:keyfind(Client, 2, Table) of
                    false ->
                        Ref = erlang:monitor(process, Client),
                        {{ok, 'nonode@nohost'}, TS#{TableName := [{Ref, Client} | Table]}};
                    _ ->
                        {{error, {already_exists, {table, TableName, simple}}}, TS}
                end;
            _ ->
                Ref = erlang:monitor(process, Client),
                {{ok, 'nonode@nohost'}, TS#{TableName => [{Ref, Client}]}}
        end,
    {reply, Result, State#state{table_subscribers = UpdatedTabSubs}};
handle_call({unsubscribe, table, TableName, simple}, {Client, _Tag}, State) ->
    TS = State#state.table_subscribers,
    {Result, UpdatedTabSubs} =
        case TS of
            #{TableName := Table} ->
                case lists:keyfind(Client, 2, Table) of
                    {Ref, Client} = RefClient ->
                        erlang:demonitor(Ref),
                        {{ok, 'nonode@nohost'}, TS#{TableName := lists:delete(RefClient, Table)}};
                    false ->
                        {{ok, 'nonode@nohost'}, TS}
                end;
            _ ->
                {{error, badarg}, TS}
        end,
    {reply, Result, State#state{table_subscribers = UpdatedTabSubs}};
handle_call(_msg, _from, State) ->
    {reply, error, State}.

handle_cast(_msg, State) ->
    {reply, error, State}.

handle_info({'DOWN', Ref, process, Pid, _Status}, State) ->
    RefPid = {Ref, Pid},
    CleanedUpTS = maps:map(
        fun(_K, V) -> lists:delete(RefPid, V) end, State#state.table_subscribers
    ),
    {noreply, State#state{table_subscribers = CleanedUpTS}};
handle_info(_msg, State) ->
    {noreply, State}.

maybe_send_event(TS, TableName, Event) ->
    case TS of
        #{TableName := Table} -> lists:foreach(fun({_Ref, Pid}) -> Pid ! Event end, Table);
        _ -> ok
    end.

remove_table_subscribers(TS, TableName) ->
    case TS of
        #{TableName := Subscribers} ->
            lists:foreach(fun({Ref, _Pid}) -> erlang:demonitor(Ref) end, Subscribers),
            maps:remove(TableName, TS);
        _ ->
            TS
    end.
