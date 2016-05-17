%% format output
-define(THROW_ERR(Err), erlang:throw({error, Err})).
-define(EXIT(Err), erlang:exit(Err)).

-define(DEBUG_MSG(), logger:debug_msg(("~w: ~w DEBUG", [?MODULE, ?LINE])).
-define(DEBUG_MSG(Format), logger:debug_msg((Format, [])).
-define(DEBUG_MSG(Format, Args), logger:debug_msg((Format, Args)).
-define(INFO_MSG(Format), logger:info_msg((Format, [])).
-define(INFO_MSG(Format, Args), logger:info_msg((Format, Args)).
-define(ERROR_MSG(Format), logger:error_msg((Format, [])).
-define(ERROR_MSG(Format, Args), logger:error_msg((Format, Args)).
