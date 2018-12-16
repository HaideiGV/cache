-module(mnesia_cache_app).
-behaviour(application).

-export([
	start/2, 
	stop/1, 
	init_db/0, 
	get/1,
	set/2, set/3, get_timestamp/0
]).

-record(data, {key, value, ttl, setup_date}).

start(_Type, _Args) -> 
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia_cache_sup:start_link().

stop(_State) ->
	mnesia:stop(),
	ok.

table_exists(TableName) ->
	Tables = mnesia:system_info(tables),
	lists:member(TableName, Tables).

get_timestamp() ->
  {Mega, Sec, _} = os:timestamp(),
  (Mega*1000000 + Sec).

% Test dataset for quick check.
load_test_data() ->
	SetupDate = get_timestamp(),
	D1 = #data{key='key1', value='value1', ttl=60, setup_date=SetupDate},
	D2 = #data{key='key2', value='value2', ttl=120, setup_date=SetupDate},
	D3 = #data{key='key3', value='value3', ttl=240, setup_date=SetupDate},
	D4 = #data{key='key4', value='value4', ttl=360, setup_date=SetupDate},
	D5 = #data{key='key5', value='value5', ttl=0, setup_date=SetupDate},
	D6 = #data{key='key6', value='value6', ttl=infinity, setup_date=SetupDate},
	Fixtures = fun() ->
		mnesia:write(D1),
		mnesia:write(D2),
		mnesia:write(D3),
		mnesia:write(D4),
		mnesia:write(D5),
		mnesia:write(D6)
	end,
	mnesia:transaction(Fixtures).

create_data_table() ->
	case table_exists(data) of
		false ->
			mnesia:create_table(data, [{attributes, record_info(fields, data)}]);
		true -> 
			ok
	end.

init_db() -> 
	create_data_table(),
	load_test_data().

is_ttl_expired(infinity, _) -> false;
is_ttl_expired(0, _) -> false;
is_ttl_expired(Ttl, SetupDate) ->
	CurrentTime = get_timestamp(),
	case CurrentTime - SetupDate  > Ttl of
		true -> true;
		false -> false
	end.

get_value_from_row([]) ->
	{error, not_found};
get_value_from_row([#data{key=Key, value=Value, ttl=Timeout, setup_date=SetupDate}]) ->
	case is_ttl_expired(Timeout, SetupDate) of
		true -> 
			mnesia:dirty_delete({data, Key}),
			{error, not_found};
		false -> {ok, Value}
	end.

get(Key) ->
	DirtyList = mnesia:dirty_read(data, Key),
	get_value_from_row(DirtyList).

set(Key, Value) ->
	NewRow = #data{key=Key, value=Value, ttl=60, setup_date=get_timestamp()}, 
	mnesia:dirty_write(data, NewRow).

set(Key, Value, [{ttl, Ttl}]) ->
	NewRow = #data{key=Key, value=Value, ttl=Ttl, setup_date=get_timestamp()}, 
	mnesia:dirty_write(data, NewRow).
