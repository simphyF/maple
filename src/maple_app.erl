%% game application

-module(maple_app).
-compile(export_all).
-behaviour(application).

start(_Type, _Args) ->
	{_, Pid} = maple_sup:start_link(),
	lists:foreach(
		fun({Msg, Fun}) ->
			io:format("starting ~w...... ~50s", [Msg]),
			Fun(),
			io:format("[ \033[1;32mOK\033[0m ]"),
		end,
		{"FILTER_WORDS", filter_words:start()},
		[{"APP_CONFIG", app_config:load_all()},
		{"LOG", log_handle:start()},
		{"DB", db:start()},
		{"GATEWAY", gateway:start()},
		{"MAPLE_SERVER", maple:start()}
		]
	),
	{ok, Pid}.

stop(_) ->
	ok.
