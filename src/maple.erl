%% game server

-module(maple).
-behaviour(gen_server).
-compile(export_all).

start() ->
	maple_sup:start_child(?MODULE).

start_link(Spec) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Spec, {timeout, 1000}).

init(Spec) ->
	{ok, Id}.	
