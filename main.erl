-module(main)
-compile(export_all).
-include("main.hrl").

start() ->
	try 
		start_application(?SERVER_APPS)
	after
		timer:sleep(1000)
	end.

stop() ->
	do_something_save(),
	erlang:halt().

do_something_save() ->
	ok.
	
start_application(Apps) ->
	manager_application(fun lists:foldl/3, fun application:start/1, fun application:stop/1, already_started, cannot_start_application, Apps).

stop_application(Apps) ->
	manager_application(fun lists:foldr/3, fun application:stop/1, fun application:start/1, not_started, cannot_stop_application, Apps).

manager_application(Iterator, Do, Undo, SkipError, ErrorTag, Apps) ->
	Iterator(
		fun(App, Acc) ->
			case Do(App) of
				ok -> [App|Acc];
				{error, {SkipError, _}} -> Acc;
				{error, Reason} ->
					lists:foreach(Undo, Acc),
					throw({error, {ErrorTag, App, Reason}})
			end
		end,
		Apps).

