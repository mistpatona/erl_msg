-module(notif).

-export([loop/0,notifier/0]).

loop() ->
	receive
		X -> io:format("Notification: ~p~n",[X])
	end,
	loop().

notifier() -> spawn_link(fun loop/0).


