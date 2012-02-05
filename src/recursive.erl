-module(recursive).
-export([fac/1, len/1]).

fac(N) when N == 0 -> 1;
fac(N) when N > 0 ->
    N * fac(N-1).


len([]) -> 0;
len([_|X])->
    1 + len(X).
