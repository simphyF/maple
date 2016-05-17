-module(log).
-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {fd, file}).

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
	process_flag(trap_exit, true),
	erlang:send_after(common_tool:get_sec_to_next_day() * 1000, ?MODULE, new_file),
	new_file(#state{}).

handle_call(Request, From, State) ->
	{reply, _, State}.

handle_cast(Request, State) ->
	{noreply, State}.

handle_info(new_file, State) ->
	erlang:send_after(common_tool:get_sec_to_next_day() * 1000, ?MODULE, new_file),
	{_, NewState} = new_file(State),
	{noreply, State};

handle_info({log_msg, Event}, State) ->
	write_event(State#state.fd, {common_tool:format_time(), Event}),	
	{noreply, State};

handle_info(_, State) ->
	{noreply, State}.

terminate(Reason, State) ->	
	ok.
	
new_file(State) ->
	FileName = get_file_name(),
	case file:open(FileName, [append, raw]) of
		{ok, Fd} ->
			{ok, #state{fd = Fd, file = FileName}};
		Error ->
			io_lib:format("open file failed, Error = ~p~n", [Error]),
			Error
	end.		

get_file_name() ->
	{Y, M, D} = common_tool:date(),
	filename:absname("") ++ io_lib:format("/logs/p~w_s~w/~w_~.2.0w_~.2.0w.log", [app_config:get(agent_id), app_config:get(server_id)], Y, M, D).

write_event(Fd, {Time, {_Type, _GL, {Pid, Format, Args}}}) ->
    case catch io_lib:format(add_node(Format, Pid), Args) of
		S when is_list(S) ->
		    file:write(Fd, io_lib:format(Time ++ S, []));
		_ ->
	 	   F = add_node("ERROR: ~p - ~p~n", Pid),
	  	  file:write(Fd, io_lib:format(Time ++ F, [Format, Args]))
   	 end;
write_event(_, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%write_event(Fd, {T, {error, _GL, {Pid, Format, Args}}}) ->
%%    case catch io_lib:format(add_node(Format,Pid), Args) of
%%        S when is_list(S) ->
%%            file:write(Fd, io_lib:format(T ++ S, []));
%%        _ ->
%%            F = add_node("ERROR: ~p - ~p~n", Pid),
%%            file:write(Fd, io_lib:format(T ++ F, [Format,Args]))
%%    end;
%%write_event(Fd, {T, {emulator, _GL, Chars}}) ->
%%    case catch io_lib:format(Chars, []) of
%%        S when is_list(S) ->
%%            file:write(Fd, io_lib:format(T ++ S, []));
%%        _ ->
%%            file:write(Fd, io_lib:format(T ++ "ERROR: ~p ~n", [Chars]))
%%    end;
%%write_event(Fd, {T, {info, _GL, {Pid, Info, _}}}) ->
%%    file:write(Fd, io_lib:format(T ++ add_node("~p~n",Pid), [Info]));
%%write_event(Fd, {T, {error_report, _GL, {Pid, std_error, Rep}}}) ->
%%    S = format_report(Rep),
%%    file:write(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []));
%%write_event(Fd, {T, {info_report, _GL, {Pid, std_info, Rep}}}) ->
%%    S = format_report(Rep),
%%    file:write(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []));
%%write_event(Fd, {T, {info_msg, _GL, {Pid, Format, Args}}}) ->
%%    case catch io_lib:format(add_node(Format,Pid), Args) of
%%        S when is_list(S) ->
%%            file:write(Fd, io_lib:format(T ++ S, []));
%%        _ ->
%%            F = add_node("ERROR: ~p - ~p~n", Pid),
%%            file:write(Fd, io_lib:format(T ++ F, [Format,Args]))
%%    end;
%%write_event(_, _) ->
%%    ok.
%%
%%format_report(Rep) when is_list(Rep) ->
%%    case string_p(Rep) of
%%        true ->
%%            io_lib:format("~s~n",[Rep]);
%%        _ ->
%%            format_rep(Rep)
%%    end;
%%format_report(Rep) ->
%%    io_lib:format("~p~n",[Rep]).
%%
%%format_rep([{Tag,Data}|Rep]) ->
%%    io_lib:format("    ~p: ~p~n",[Tag,Data]) ++ format_rep(Rep);
%%format_rep([Other|Rep]) ->
%%    io_lib:format("    ~p~n",[Other]) ++ format_rep(Rep);
%%format_rep(_) ->
%%    [].
%%
add_node(X, Pid) when is_atom(X) ->
    add_node(atom_to_list(X), Pid);
add_node(X, Pid) when node(Pid) /= node() ->
    lists:concat([X,"** at node ",node(Pid)," **~n"]);
add_node(X, _) ->
    X.
%%
%%string_p([]) ->
%%    false;
%%string_p(Term) ->
%%    string_p1(Term).
%%
%%string_p1([H|T]) when is_integer(H), H >= $\s, H < 255 ->
%%    string_p1(T);
%%string_p1([$\n|T]) -> string_p1(T);
%%string_p1([$\r|T]) -> string_p1(T);
%%string_p1([$\t|T]) -> string_p1(T);
%%string_p1([$\v|T]) -> string_p1(T);
%%string_p1([$\b|T]) -> string_p1(T);
%%string_p1([$\f|T]) -> string_p1(T);
%%string_p1([$\e|T]) -> string_p1(T);
%%string_p1([H|T]) when is_list(H) ->
%%    case string_p1(H) of
%%        true -> string_p1(T);
%%        _    -> false
%%    end;
%%string_p1([]) -> true;
%%string_p1(_) ->  false.
