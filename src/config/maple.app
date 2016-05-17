[
	{application, sample, 
	[
		{description, "maple test"},
		{vsn, "1.0"},
		{modules, [game_server]},	
		{registered, [maple_app]},	
		{applications, [kernel, stdlib, sasl]},	
		{mod, {maple_app, []}},	
		{start_phases, []},	
		{env, []},	
		{included_applications, []}	
	]
]

