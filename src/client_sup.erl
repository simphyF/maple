-module(gateway).
-compile(export_all).
-behaviour(supervisor).

start_link(_) ->
	supervisor:start_link(?MODULE, []).

init(_) ->
	{ok, {one_for_all, 10, 10}, {client, {client, start_link, []}, transient, 1000, worker, [client]}}.
