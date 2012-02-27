-module(road).
-compile(export_all).


main(Filename)->
    {ok, Binary} = file:read_file(Filename),
    Map = parse_map(binary_to_list(Binary)),
    io:format("~p~n", [optimal_path(Map)]),
    erlang:halt(0).


% Read file and prepare for input as tuples {A1,B1,X1}, {A2,B2,X2}, {An,Bn,Xn}
generate_tuples([], Acc) -> Acc;
generate_tuples([A,B,X|Rest], Acc) ->
     generate_tuples(Rest, [{A,B,X}|Acc]).

% Take each element in the file and convert to integer
parse_map(L)->
    lists:reverse(generate_tuples([list_to_integer(X) || X <- string:tokens(L, "\r\n\r")], [])).


shortest_step({A,B,X}, {{DistA,PathA}, {DistB,PathB}}) ->
    OptA1 = {DistA + A, [{a,A}|PathA]},
    OptA2 = {DistB + B + X, [{x,X}, {b,B}|PathB]},
    OptB1 = {DistB + B, [{b,B}|PathB]},
    OptB2 = {DistA + A + X, [{x,X}, {a,A}|PathA]},
    {erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)}.

optimal_path(Map) ->
    {A,B} = lists:foldl(fun shortest_step/2, {{0,[]}, {0,[]}}, Map),
    {_Dist,Path} = if hd(element(2,A)) =/= {x,0} -> A;
    hd(element(2,B)) =/= {x,0} -> B
    end,
    lists:reverse(Path).
