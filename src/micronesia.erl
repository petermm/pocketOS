-module(micronesia).

-behavior(gen_server).

-export([
    start/0,
    create_table/1,
    all/1,
    read/1,
    write/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {tables = #{}}).

start() ->
    gen_server:start({local, micronesia}, ?MODULE, [], []).

create_table(Name) ->
    gen_server:call(micronesia, {create_table, Name}).

all(Name) ->
    gen_server:call(micronesia, {all, Name}).

read({_Name, _Id} = T) ->
    gen_server:call(micronesia, {read, T}).

write(Record) ->
    gen_server:call(micronesia, {write, Record}).

init([]) ->
    {ok, #state{}}.

handle_call({create_table, Name}, _From, State) ->
    Tables = State#state.tables,
    WithNameTable = Tables#{Name => []},
    {reply, ok, State#state{tables = WithNameTable}};
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
handle_call({read, {TableName, Id}}, _From, State) ->
    Tables = State#state.tables,
    case Tables of
        #{TableName := Table} ->
            % lists:keyfind is not good
            case lists:keyfind(Id, 1, Table) of
                false ->
                    {reply, [], State};
                Value ->
                    Result = erlang:insert_element(1, Value, TableName),
                    {reply, [Result], State}
            end;
        _ ->
            {reply, error, State}
    end;
handle_call({write, Record}, _From, State) ->
    TableName = element(1, Record),
    Columns = erlang:delete_element(1, Record),
    Tables = State#state.tables,
    case Tables of
        #{TableName := Table} ->
            NewTable = [Columns | Table],
            {reply, ok, State#state{tables = Tables#{TableName := NewTable}}};
        _ ->
            {reply, error, State}
    end;
handle_call(_msg, _from, State) ->
    {reply, error, State}.

handle_cast(_msg, State) ->
    {reply, error, State}.

handle_info(_msg, State) ->
    {noreply, State}.
