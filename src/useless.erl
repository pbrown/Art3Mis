-module(useless).
-export([add/2, hello/0, greet_and_add_two/1]).
-import(io, [format/1]).
-author("Pooja Garg").


add(A,B) ->
    A + B.

%% Show Greetings
%% io:format is the standard function used to output text.
hello() ->
    format("Hello World !\n").


greet_and_add_two(X) ->
    hello(),
    add(X,2).