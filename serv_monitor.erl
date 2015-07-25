-module(serv_monitor).

% example: serv_monitor:monitor_server('srv@harlot.internal').

-compile(export_all).

-define(TIMEOUT,5000).
-define(WORKINGTIMEOUT,?TIMEOUT*12).

mon(N) -> 
	case net_adm:ping(N) of
		pong -> mon_up(N);
		_ ->  	%io:format("Mon: ~p is down, waiting ~p s~n",[N,?TIMEOUT]),
			receive
			after ?TIMEOUT -> void
			end,
			mon(N)
	end.

mon_up(N) ->
	%io:format("Mon: ~p is up, setting waiter~n",[N]),
	monitor_node(N,true),
	wait_exit(N),
	mon(N).

wait_exit(N) ->
	receive
		{nodedown, N} -> mon(N);
		shutdown -> ok;
		_	-> wait_exit(N)
	after ?WORKINGTIMEOUT -> void
	end,
	mon(N).

monitor_server(N) ->
	spawn_link(?MODULE,mon,[N]).
