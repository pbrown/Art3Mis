-module(calc).
-compile([export_all]).


%% Reverse Polish Notation
rpn(L) when is_list(L)->
    io:format("List Looks like : ~s ~n", [L]),
    [Result]= lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
    Result.


rpn("+", [N1,N2|S]) -> [N2+N1|S];
rpn("-", [N1,N2|S]) -> [N2-N1|S];
rpn("*", [N1,N2|S]) -> [N2*N1|S];
rpn("/", [N1,N2|S]) -> [N2/N1|S];

rpn(X, Stack)->
      io:format("Stack Looks like :~p ~n", [Stack]),
      io:format("X Looks like : ~p ~n", [X]),
      [read(X)|Stack].

read(N) ->
        case string:to_float(N) of
            {error,no_float} -> list_to_integer(N);
            {F,_} -> F
        end.


rpn_test()->
    5 = rpn("2 3 +"),
    ok = try
            rpn("90 45 45 66 12 + * - /")
         catch
            error: {badmatch, [_|_]}->ok
         end,
    ok.


%% Polish Notation

pn(L) when is_list(L) ->
   [Result] = lists:foldl(fun pn/2, [], string:tokens(L, " ")),
   Result.


pn("+", [N1,N2|Rest]) -> io:format("Rest ~p ~n", [Rest]), [N2+N1|Rest];

pn(X, Stack)->
    io:format("Stack Looks like :~p ~n", [Stack]),
    io:format("X Looks like : ~p ~n", [X]),
    [read(X)|Stack].

read_pn(N) ->
       case string:to_float(N) of
            {error,no_float} -> list_to_integer(N);
            {F,_} -> F
        end.



pnot(L) when is_list(L) ->
    poperate(L, []).


poperate([H|T], Stack) ->
    io:format("This is the state of Stack ~p ~n", [Stack]),
    case string:to_integer(H) of
       {error, no_integer}->list_to_integer(H);
       {F,_} -> F
    end.


