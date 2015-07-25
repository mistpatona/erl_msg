-module(notif).

-export([loop/0,notifier/0]).

loop() ->
	receive
		{message,M} -> io:put_chars(M),io:nl(),loop();
		shutdown -> ok;
		X -> io:format("Notification: ~s~n",[X]),loop()
	end.
	%loop().

notifier() -> spawn_link(fun loop/0).


