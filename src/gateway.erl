-module(gateway).
-compile(export_all).
-behaviour(supervisor).

-define(LISTEN_OPTS, [ 
    binary,
    {packet, 0},
	{alive, false},
    {reuseaddr, true},
    {delay_send, true},
    {backlog, 1024},
    {exit_on_close, false},
    {send_timeout, 30000}
]).

start() ->
	ListenArgs = [app_config:get(game_port), app_config:get(tcp_acceptor_num), ?LISTEN_OPTS],
	supervisor:start_child(maple_sup, {listener_sup, {listener_sup, start_link, ListenArgs}, transient, infinity, supervisor, [listener_sup]}),
	supervisor:start_child(maple_sup, {client_sup, {client_sup, start_link, []}, transient, infinity, supervisor, [client_sup]}).
	
