-module(mnesia_cache_app).
-behaviour(application).

-export([
	start/2, 
	stop/1, 
	init_db/0, 
	get/1, 
	set/2, 
	set/3, 
	delete/1,
	get_timestamp/0
]).

-define(DEFAULT_TTL, 60).
-define(ZERO_TTL, 0).
-define(INFINITY_TTL, infinity).
-record(data, {key, value, ttl, setup_date}).

start(_Type, _Args) -> 
	mnesia:create_schema([node()]),
	mnesia:start(),
	timer:start_link(),
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
	D1 = #data{key=key1, value=value1, ttl=?DEFAULT_TTL, setup_date=SetupDate},
	D2 = #data{key=key2, value=value2, ttl=?DEFAULT_TTL * 2, setup_date=SetupDate},
	D3 = #data{key=key3, value=value3, ttl=?DEFAULT_TTL * 3, setup_date=SetupDate},
	D4 = #data{key=key4, value=value4, ttl=?DEFAULT_TTL * 4, setup_date=SetupDate},
	D5 = #data{key=key5, value=value5, ttl=?ZERO_TTL, setup_date=SetupDate},
	D6 = #data{key=key6, value=value6, ttl=?INFINITY_TTL, setup_date=SetupDate},
	Fixtures = fun() ->
		mnesia:write(D1),
		mnesia:write(D2),
		mnesia:write(D3),
		mnesia:write(D4),
		mnesia:write(D5),
		mnesia:write(D6)
	end,
	mnesia:transaction(Fixtures),
	delete_after(key1, 5),
	delete_after(key2, 10),
	delete_after(key3, 15),
	delete_after(key4, 20),
	ok.

create_data_table() ->
	case table_exists(data) of
		false ->
			mnesia:create_table(data, [{attributes, record_info(fields, data)}, {type, set}]);
		true -> 
			ok
	end.

init_db() -> 
	create_data_table(),
	load_test_data().

is_ttl_expired(?INFINITY_TTL, _) -> false;
is_ttl_expired(?ZERO_TTL, _) -> false;
is_ttl_expired(Ttl, SetupDate) ->
	CurrentTime = get_timestamp(),
	CurrentTime - SetupDate  > Ttl.

get_value_from_row([]) ->
	{error, not_found};
get_value_from_row([#data{key=Key, value=Value, ttl=Timeout, setup_date=SetupDate}]) ->
	case is_ttl_expired(Timeout, SetupDate) of
		true -> 
			delete(Key),
			{error, not_found};
		false -> {ok, Value}
	end.

get(Key) ->
	DirtyList = mnesia:dirty_read(data, Key),
	get_value_from_row(DirtyList).

set(Key, Value) ->
	NewRow = #data{key=Key, value=Value, ttl=?DEFAULT_TTL, setup_date=get_timestamp()}, 
	delete_after(Key, ?DEFAULT_TTL),
	mnesia:dirty_write(data, NewRow).

set(Key, Value, [{ttl, Ttl}]) ->
	NewRow = #data{key=Key, value=Value, ttl=Ttl, setup_date=get_timestamp()}, 
	delete_after(Key, Ttl),
	mnesia:dirty_write(data, NewRow).

delete(Key) ->
	Fun = fun() -> mnesia:delete({data, Key}) end,
	mnesia:transaction(Fun),
	io:fwrite("Deleted: ~w~n", [Key]).

delete_after(Key, Ttl) ->
	timer:apply_after(Ttl * 1000, ?MODULE, delete, [Key]),
	io:fwrite("Delayed: ~w for ~w seconds.~n", [Key, Ttl]).