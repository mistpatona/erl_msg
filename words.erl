-module(words).

-export([words/1]).

-import(lists,[reverse/1]).


words(S) -> 
	words([],S,[]).

is_space(C) -> C =:= $\s.

words(Tmp,[C|Rest],Ans) ->
	case is_space(C) of
		false -> words([C|Tmp],Rest,Ans);
		_ -> 	 words([],Rest,cons_reverse_if_not_null(Tmp,Ans))
	end;
words(Tmp,[],Ans) -> reverse(cons_reverse_if_not_null(Tmp,Ans)).

cons_reverse_if_not_null([],R) -> R;
cons_reverse_if_not_null(H,R) -> [reverse(H)|R].
