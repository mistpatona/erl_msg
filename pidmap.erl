-module(pidmap).
-export([build_pid_map/1,testarray/0]).

testarray() -> [{0,0,a},{1,1,a},{2,2,b},{3,3,b},{9,9,a}].

build_pid_map(L) ->
	%  O(n^2) complexity, not good
	Logins = lists:usort([ Login || {_,_,Login} <- L ]),
	[ {Login,[ NPid || {_,NPid,Login1} <- L, Login =:= Login1 ] } || Login <- Logins ].

