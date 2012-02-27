#! /usr/bin/env escript
-module(useless).
-export([add/2, main/1]).
-author("Pooja Garg").
-mode(compile).

main([String])->
	T = string:tokens(String, " "),
	N1 = list_to_integer(lists:nth(1,T)),
	N2 = list_to_integer(lists:nth(2,T)),
	Sum =add(N1, N2),
	io:format("Sum of 2 numbers: ~p ~n", [Sum]).

add(A,B)->
	A+B.

