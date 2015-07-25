-module(msg_server).
-behaviour(reg_server).

-export([start_link/0, start/0]).

% callbacks for gen_server:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     	terminate/2, 
	code_change/3]).

-export([login/2,logout/1,listonline/1,listallusers/1,whoami/1,
	registernotifications/1,
	sendmessage/3,fetchmessage/2,getmessagelistforme/1,
	getsessionwith/2]).


-record(state, {
	users,online,messages,sndrs,rcvrs,notify_login,notify_pid}).

start() ->
	start_link(),
	create_mails().

create_mails()->
	login("Serge",self()),
	sendmessage(self(),"Pavian","hello monkey!"),
	login("Pavian",self()),
	sendmessage(self(),"Serge","hello white-faced"),
	sendmessage(self(),"Makaka","hello"),
	login("Serge",self()),
	sendmessage(self(),"Pavian","uhhu-uh!"),
	sendmessage(self(),"Boris","uhhu-uh!"),
	login("Pavian",self()),
	sendmessage(self(),"Serge","uh-uh-uh"),
	sendmessage(self(),"Boris","uh-uh-uh"),
	sendmessage(self(),"Makaka","uh-uh-uh"),
	login("Serge",self()),
	sendmessage(self(),"Pavian","uuuhhhh!"),
	logout(self()).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
%% what are the arguments menings here?

-define(SERVER, global:whereis_name(?MODULE)).

init([]) ->
	process_flag(trap_exit,true), %we need to catch exited pids
	io:format("~w Initing database~n",[self()]),
	M = ets:new(?MODULE,[set]),
	ets:insert(M,{lastMid,0}),
	{ok, #state{
		users 		= ets:new(?MODULE,[set]),
		online 		= ets:new(?MODULE,[set]),
		messages 	= M, 
		sndrs 		= ets:new(?MODULE,[bag]),
		rcvrs 		= ets:new(?MODULE,[bag]),
		notify_login	= ets:new(?MODULE,[bag]),
		notify_pid	= ets:new(?MODULE,[bag])
		}
	}.

handle_call({login,Login,Pid}, _From, State) ->
	%first check for previous logins and remove notifiers:
	ets:delete(State#state.notify_pid,Pid),

	%there will be some trouble if client will login second time without logout:
	%  his notification handlers will not be deleted.
	%  and if client simultaneously logins from several points,
	%  his notification handlers when he logs out 
	%  will be treated as  belonging to a single point and all be deleted

	ets:insert(State#state.users, {Login}),
	ets:insert(State#state.online, {Pid,Login}),
	link(Pid),  %catch if pid exits
	io:format("Login: ~p from ~w~n",[Login,Pid]),
	{reply,ok,State};

handle_call({logout,Pid}, _From, State) ->
	unlink(Pid),
	Rec=ets:lookup(State#state.online,Pid),
	ets:delete(State#state.online,Pid),
	Login = hd(Rec),

	ets:delete(State#state.notify_login,Login),
	ets:delete(State#state.notify_pid,Pid),

	io:format("Logout: ~p from ~w~n",[Login,Pid]),
	{reply,ok,State};

handle_call({register_callback,UserPid,NotifiedPid}, _From, State) ->
	Login = get_login(UserPid,State),
	ets:insert(State#state.notify_login,{Login,NotifiedPid}),
	ets:insert(State#state.notify_pid,{UserPid,NotifiedPid}),
	io:format("Registered for notifications: Pid:~p User:~p~n",[NotifiedPid,Login]),
	{reply,ok,State};

handle_call({who_am_i,Pid}, _From, State) ->
	Login = get_login(Pid,State),
	{reply,{ok,Login},State};

handle_call({who_is_online,_Pid}, _From, State) ->
	T=ets:tab2list(State#state.online),
	%R=[L || {P,L} <- T, P =/= Pid], %except asker
	R=[L || {_P,L} <- T],
	{reply,{ok,R},State};

handle_call({all_users,_Pid}, _From, State) ->
	T=ets:tab2list(State#state.users),
	R=[L || {L} <- T],
	{reply,{ok,R},State};

handle_call({send,Pid,To,Body}, _From, State) ->
	[{_P,From}]=ets:lookup(State#state.online,Pid),
	Mid = case ets:lookup(State#state.messages,lastMid) of
		[{_,OldMid}] -> OldMid+1;
		_ -> erlang:error("No lastMid record found in messages table")
	end,
	ets:insert(State#state.messages,{lastMid,Mid}),
	ets:insert(State#state.messages,{Mid,To,From,Body}), % timestamp not needed, messages can be sorted by message id
	ets:insert(State#state.sndrs,{From,Mid}),
	ets:insert(State#state.rcvrs,{To,Mid}),
	NoteMsg=lists:flatten([From,": ",Body]),
	notify_remotes(To,NoteMsg,State),
	{reply,{ok,Mid},State};

handle_call({fetch,Pid,Mid}, _From, State) ->
	Ms = ets:lookup(State#state.messages,Mid),
	M = case Ms of
		[X] -> X;
		[] -> erlang:error("No message with such id: ~p",[Mid]);
		_ -> erlang:error("More than one message with such id") 
	end,
	%io:format("Found message: ~p~n",[M]),
        {_Mid,To,Fr,_Body} = M, 
	[{_P,L}] = ets:lookup(State#state.online,Pid),
	%io:format("You:~p Sender:~p Recepient:~p~n",[L,Fr,To]),
	case (L =/= To) andalso ( L =/= Fr) of
		true -> 
			{reply,{error,"not your letter"},State};
		_ -> 	{reply,{ok,M},State}
	end;

handle_call({get_message_list_for_me,Pid}, _From, State) ->
	Login = get_login(Pid,State),
	L = ets:lookup(State#state.rcvrs,Login),
	M = [X || {_,X} <- L],
	{reply,{ok,M},State};

handle_call({get_session_with,Pid,Friend}, _From, State) ->
	Login = get_login(Pid,State),
	Lout1 = ets:lookup(State#state.sndrs,Login),
	Lout2 = ets:lookup(State#state.rcvrs,Friend),
	Lout =    intersect(mapsnd(Lout1), mapsnd(Lout2)),
	Lin1  = ets:lookup(State#state.sndrs,Friend),
	Lin2  = ets:lookup(State#state.rcvrs,Login),
	Lin   = intersect(mapsnd(Lin1), mapsnd(Lin2)),
	L = lists:sort(Lin ++ Lout),
	{reply,{ok,L},State};

handle_call(Msg, _From, State) ->
	io:format("!!!Surprise Call: ~w from ~w~n",[Msg,_From]),
	{reply,ok,State}.


handle_info({'EXIT',Pid,Why},State) ->
	io:format("EXIT info: ~w, reason: ~w~n",[Pid,Why]),
	handle_call({logout,Pid}, unknown, State),
	{noreply,State};

handle_info(Info,State) ->
	io:format("!!!Surprise Info: ~w~n",[Info]),
	{noreply,State}.

handle_cast(stop,State) ->
	{stop,normal,State};

handle_cast(Msg,State) ->
	io:format("!!!Surprise Cast: ~w~n",[Msg]),
	{noreply,State}.


terminate(Why,State) ->
	io:format("~w Terminating: ~w~n",[self(),Why]),
	{ok,State}.

code_change(_,State,_) ->
	{ok,State}.

%=====================================================

get_login(Pid,State) ->
	%[{_P,Login}]=ets:lookup(State#state.online,Pid),
	case ets:lookup(State#state.online,Pid) of
		[{_P,Login}] -> Login;
		_ -> not_logged_in
	end.

notify_remotes(Login,Msg,State) ->
	L = ets:lookup(State#state.notify_login,Login),
	[ X ! Msg || {_,X} <- L ].

intersect(M,N) ->
	I=sets:intersection(sets:from_list(M),sets:from_list(N)),
	sets:to_list(I).

mapsnd(L) -> [X || {_,X} <-L].

%=====================================================

login(L,Pid) ->
	gen_server:call(?SERVER,{login,L,Pid}).

logout(Pid) ->
	gen_server:call(?SERVER,{logout,Pid}).

whoami(Pid) ->
	gen_server:call(?SERVER,{who_am_i,Pid}).

listonline(Pid) ->
	gen_server:call(?SERVER,{who_is_online,Pid}).

listallusers(Pid) ->
	gen_server:call(?SERVER,{all_users,Pid}).

sendmessage(Pid,To,Body) ->
	gen_server:call(?SERVER,{send,Pid,To,Body}).

fetchmessage(Pid,Mid) ->
	gen_server:call(?SERVER,{fetch,Pid,Mid}).

getmessagelistforme(Pid) ->
	gen_server:call(?SERVER,{get_message_list_for_me,Pid}).

getsessionwith(Pid,Friend) ->
	gen_server:call(?SERVER,{get_session_with,Pid,Friend}).

registernotifications(Pid)->
	gen_server:call(?SERVER,{register_callback,self(),Pid}).
