-module(mnesia_cache_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
		#{
			id => mnesia_cache_cleaner,
			start => {mnesia_cache_cleaner, start_link, []},
			restart => permanent,
			shutdown => brutal_kill,
			type => worker,
			modules => [mnesia_cache_cleaner]
		}
	],
	{ok, {{one_for_one, 1, 5}, Procs}}.
