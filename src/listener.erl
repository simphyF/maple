-module(listener_sup).
-compile(export_all).
-behaviour(supervisor).

start_link() ->
	supervisor:start_link(?MODULE, {app_config:get(game_port), app_config:get(tcp_acceptor_num)}).

init({Port, Num}) ->
	{ok, {one_for_all, 10, 10}, [ 
		{acceptor_sup, {acceptor, start_link, [Port, Num]}, transient, infinity, supervisor, [acceptor_sup]},
		{listener, {listener, start_link, []}, transient, 1000, worker, [listener]}
	]}.

