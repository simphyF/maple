-module(app_config).
-compile(export_all).
-include("main.hrl").
-include("format.hrl").

load_all() ->
	lists:foreach(fun(E) -> load(E) end, ?SERVER_APPS).		

load(App) ->
	EnvList = get_config(App),
	lists:foreach(fun({Key, Value}) -> application:set_env(App, Key, Value) end, EnvList).		

get_config(App) ->
	case file:consult("../config/config") of
		{ok, AppConfigList} ->
			case lists:keyfind(App, #config.server, AppConfigList) of
				false ->
					[];
				R ->
					R#config.env
			end;
		_ ->
			[]
	end.

get(Key) ->
	get(maple, key).	

get(App, Key) ->
	case application:get_env(App, Key) of
		{ok, V} ->
			V;
		_ ->
			undefined
	end.
