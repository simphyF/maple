-module(listener_sup).
-compile(export_all).
-behaviour(supervisor).

start_link(Args) ->
	supervisor:start_link(?MODULE, [Args]).

init(Args) ->
	{ok, {one_for_all, 10, 10}, [ 
		{acceptor_sup, {acceptor, start_link, Args}, transient, infinity, supervisor, [acceptor_sup]},
		{listener, {listener, start_link, []}, transient, 1000, worker, [listener]}
	]}.

