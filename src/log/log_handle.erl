-module(log_handle).
-behaviour(gen_event).
-export([start/0, init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).
-define(LOGMODULE, "error_logger").
-define(LOG_LEVELS,[
	{0, no_log},
	{1, error},
	{2, info},
	{3, debug}
	]).

start() ->
	load_logger_src().	
	error_logger:add_report_handle(?MODULE).

load_logger_src() ->
  	try
		Level = get_logger_level(),
        {Mod,Code} = dynamic_compile:from_string(get_logger_src(Level)),
        code:load_binary(Mod, ?LOGMODULE ++ ".erl", Code)
    catch
        Type:Error -> ?EXIT({load_logger_src_failed, Type, Error})
    end.

get_logger_level() ->	
	LogLevel = app_config:get(logger_level)),
	case lists:keysearch(LogLevel, 1, ?LOG_LEVELS) of
		{value, _} ->
			LogLevel;
		_ ->
			?EXIT({no_such_loglevel, LogLevel})
	end.	

get_logger_src(Level) ->	
	L = integer_to_list(Level),
	"-module(logger).
	-export([debug_msg/2, info_msg/2, error_msg/2]).
	debug_msg(Format, Args) when " ++ L  ++ " >= 3 ->
			notify(info_msg, \"[D:]\" ++ Format ++ \"~n\", Args);
	debug_msg(_, _) ->
		ok.

	info_msg(Format, Args) when " ++ L  ++ " >= 2 ->
		notify(info_msg, \"[I:]\" ++ Format ++ \"~n\", Args);
	info_msg(_, _) ->
		ok.
	
	error_msg(Format, Args) when " ++ L  ++ " >= 1 ->
		notify(error, \"[E:]\" ++ Format ++ \"~n\", Args);
	error_msg(_, _) ->
		ok.

	notify(Type, Format, Args) ->
		Msg = {Type, group_leader(), {self(), Format, Args}},	
		gen_event:notify(error_logger, Msg).
	".

%% gen_event call back
init(_) ->
	log:start(),
	{ok, #state{}}.

handle_event(Event, State) ->
	erlang:send(log, {log_msg, Event}),
	{ok, State}.

handle_call(_Request, State) ->
	{ok, noreplay, State}.

handle_info(_Request, State) ->
	{ok, State}.

code_change(_, State, _) ->
	{ok, State}.

terminate(_, _) ->
	ok.
