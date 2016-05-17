%% tool
%% include deal time/string/list
-module(common_tool).
-compile(export_all).

to_string(_) ->
	ok.

to_term(_) ->
	ok.

to_int() ->
	ok.

timestamp() ->
	{MSec, Sec, _} = os:timestamp(),
	MSec * 1000 + Sec.

timestamp(DateTime) ->
	calendar:datetime_to_gregorian_seconds(calendar:local_time_to_universal_time(DateTime)) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).	

datetime() ->
	calendar:local_time().

datetime(Timestamp) ->
	calendar:now_to_local_time({Timestamp div 1000 div 1000, Timestamp rem (1000 * 1000), 0}),			

date() ->
	{Date, _} =  common_tool:datetime(),
	Date.

date(Timestamp) ->
	{Date, _} =  common_tool:datetime(Timestamp),
	Date.

time() ->
	{_, Time} = common_tool:datetime(),
	Time.	

time(Timestamp) ->
	{_, Time} = common_tool:datetime(Timestamp),
	Time.		

get_day_begin_timestamp() ->
	get_day_begin_timestamp(common_tool:datetime()).

get_day_begin_timestamp(DateTime) ->
	{Date, _} = DateTime,
	common_tool:timestamp({Date, {0, 0, 0}}).

get_day_end_timestamp() ->	
	get_day_begin_timestamp() + 86400.

get_day_end_timestamp(DateTime) ->
	get_day_begin_timestamp(DateTime) + 86400.

get_interval_sec(DateTime1, DateTime2) ->
	T1 = common_tool:timestamp(DateTime1), 	
	T2 = common_tool:timestamp(DateTime2),
	max(T1 - T2, T2 - T1).

get_sec_to_next_day() ->
	get_day_end_timestamp() - common_tool:timestamp().

get_sec_to_datetime(DateTime) ->
	max(0, common_tool:timestamp(DateTime) - common_tool:timestamp()).
	
format_time() ->
	format_time(common_tool:datetime()).

format_time(DateTime) ->
	{{Y, M, D}, {H, I, S}} = DateTime,
	io_lib:format("~w-~.2.0w-~.2.0w ~w:~.2.0w:~.2.0w", [Y, M, D, H, I, S]).
