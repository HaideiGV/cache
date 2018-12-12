-module(mnesia_cache_app).
-behaviour(application).

-export([
	start/2, 
	stop/1, 
	init_db/0, 
	get/1, 
	dirty_get/1,
	dirty_set/2
]).

-record(data, {key, value}).

start(_Type, _Args) -> 
	mnesia_cache_sup:start_link().

stop(_State) ->
	mnesia:stop(),
	ok.

table_exists(TableName) ->
	Tables = mnesia:system_info(tables),
	lists:member(TableName, Tables).

load_test_data() ->
	D1 = #data{key='key1', value='value1'},
	D2 = #data{key='key2', value='value2'},
	D3 = #data{key='key3', value='value3'},
	Fixtures = fun() ->
		mnesia:write(D1),
		mnesia:write(D2),
		mnesia:write(D3)
	end,
	mnesia:transaction(Fixtures),
	D4 = #data{key='key4', value='value4'},
	mnesia:dirty_write(data, D4).

create_data_table() ->
	case table_exists(data) of
		false ->
			mnesia:create_table(data, [{attributes, record_info(fields, data)}]);
		true -> 
			ok
	end.

init_db() -> 
	mnesia:create_schema([node()]),
	mnesia:start(),
	create_data_table(),
	load_test_data().

dirty_get(Key) ->
	DirtyList = mnesia:dirty_read(data, Key),
	case DirtyList of
		[] -> {error, not_found};
		[#data{key=Key, value=Value} | _] -> {ok, Value}
	end.

dirty_set(Key, Value) ->
	NewRow = #data{key=Key, value=Value}, 
	mnesia:dirty_write(data, NewRow).

get(Key) -> 
	Row = #data{key=Key, value= '_'},
	F = fun() -> 
		mnesia:select(data, [{Row, [], []}])
	end,
	mnesia:transaction(F).
