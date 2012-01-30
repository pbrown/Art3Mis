-module(functions).
-compile(export_all). %%Todo: Replace with selective export

head([H|_]) ->
    H.

second([_,X|_]) ->
    X.

valid_time({Date={Y,M,D}, Time={H,Min,Sec}})->
    io:format("The Date Tuple (~p) says today is: ~p/~p/~p,~n",[Date,Y,M,D]),
    io:format("The Time tuple(~p) indicates ~p:~p:~p,~n", [Time,H,Min,Sec]);
valid_time(_)->
    io:format("Stop feeding me the wrong time").


old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.

right_age(X) when X >=16, X =< 104 ->
    true;
right_age(_)->
    false.

wrong_age(X) when X <16; X>104 ->
    true;
wrong_age(_)->
    false.
