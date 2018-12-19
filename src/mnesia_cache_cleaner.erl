-module(mnesia_cache_cleaner).
-behaviour(gen_server).

-export([init/1, start_link/0, handle_call/2, handle_call/3, handle_cast/2, handle_info/2]).

-define(INTERVAL, 5000).

init(_Args) ->
    timer:send_after(?INTERVAL, self(), check).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

handle_call(_From, _State) ->
    {reply, _From, _State}.
handle_call(_, _From, _State) ->
    {reply, _From, _State}.

handle_cast(_From, _State) ->
    {noreply, _State}.

handle_info(check, _State) ->
    io:fwrite("Info called! Current time: ~w. ~n", [mnesia_cache_app:get_timestamp()]),
    timer:send_after(?INTERVAL, self(), check),
    {noreply, _State}.