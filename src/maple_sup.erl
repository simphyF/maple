%% game application, tree top

-module(maple_sup).
-compile(export_all).
-behaviour(supervisor).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init() ->
	{ok, {one_for_one, 10, 10}, []}.

