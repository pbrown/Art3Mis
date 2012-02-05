-module(recursive).
-export([fac/1, len/1, tail_fact/1, tail_fact/2, tail_len/1, tail_len/2]).

fac(N) when N == 0 -> 1;
fac(N) when N > 0 ->
    N * fac(N-1).


len([]) -> 0;
len([_|X])->
    1 + len(X).


tail_fact(N) -> tail_fact(N,1).
tail_fact(0, Accumulator) -> Accumulator;
tail_fact(N, Accumulator) when N > 0 -> tail_fact(N-1, N*Accumulator).

tail_len(X) -> tail_len(X,0).
tail_len([], Accumulator) -> Accumulator;
tail_len([_|T], Accumulator) -> tail_len(T, Accumulator+1).
