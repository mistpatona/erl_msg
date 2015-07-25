-module(login).
-behaviour(gen_fsm).

-compile(export_all). % development

-import(msg_server,[login/2,logout/1]).

start() -> gen_fsm:start_link({local,?MODULE},?MODULE,[],[]).

init(_) -> {ok,disconnected,xxx}.

disconnected(connect,State) ->
	case global:whereis_name(msg_server) of
		undefined ->{next_state,disconnected,State};
		_ -> {next_state,connected,State}
	end.

connected({login,Name},State) ->
	case msg_server:login(L,self()) of
		ok -> 	io:format("Logged in as ~p~n",[Login]),
			{next_state,loggedin,State};
		E -> 	io:format("Could not login: ~p~n",[E]),
			{next_state,connected,State}
	end.

loggedin(logout,State) ->
	msg_server:logout(self()),
	{next_state,connected,State}.
