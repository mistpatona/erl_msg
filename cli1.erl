-module(cli1).

-compile(export_all). % development

%-import(msg_server,[login/2,logout/1]).
% net_adm:ping('srv@harlot.internal').

login(Name) -> msg_server:login(Name,self()).

logout() -> msg_server:logout(self()).

listusers() ->
	All_1 = msg_server:listallusers(self()),
	{ok,All_2} = All_1,
	All = sets:from_list(All_2),
	%io:format("Debug:All users: ~p~n",[All]),
	Onl_1 = msg_server:listonline(self()),
	{ok,Onl_2} = Onl_1,
	Onl = sets:from_list(Onl_2),
	%io:format("Debug:Online users: ~p~n",[Onl]),
	Ofl = sets:subtract(All,Onl),
	S = [ ["*",N," "] || N <- sets:to_list(Onl) ],
	S2 = [ ["_",N," "] || N <- sets:to_list(Ofl) ],
	lists:flatten([S,S2]).
	
send(To,What) ->
	msg_server:sendmessage(self(),To,What).

listallmsg() ->
	msg_server:getmessagelistforme(self()).

showmsg(M) ->
	msg_server:fetchmessage(self(),M).

showhistory(Friend) ->
	msg_server:getsessionwith(self(),Friend).

showhistorytext(Friend) ->
	{ok,L} = showhistory(Friend),
	lists:foreach(fun(M) -> io:format("~p~n",[fetched_to_string(showmsg(M))]) end ,L).

fetched_to_string({ok,{_,To,Fr,Body}}) ->
	lists:flatten([Fr,"->",To," ",Body,"\n"]);
fetched_to_string(Msg) ->
	lists:flatten(["Some error in: ",Msg]).



