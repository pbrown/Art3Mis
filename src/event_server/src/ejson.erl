-module(ejson).
-compile(export_all).
-record(event, {name="", description="", pid, timeout={{1970,1,1}, {0,0,0}}}). 


parse(json, Body) ->
	io:format("In function parse json ~p~n", [Body]),
	Tokens = string:tokens(Body, "\"{},: "),
	io:format("First Tokens ~p~n", [Tokens]),	
	parse(Tokens).


parse([]) ->
	io:format("Done parsing the list~n");

parse([H|T]) ->
	case H of 
		"name" ->
			io:format("Value of name ~p~n", [H]);	
		"description" ->
			io:format("Description ~p~n", [H])
	end.

	
