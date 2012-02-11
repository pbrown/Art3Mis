-module(recursive).
-export([fac/1, len/1, tail_fact/1, tail_fact/2, tail_len/1, tail_len/2, duplicate/2, tail_duplicate/2, tail_duplicate/3,
        reverse/1, tail_reverse/1, tail_reverse/2, sublist/2]).

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


duplicate(0,_)->[];
duplicate(N,Term)->[Term|duplicate(N-1,Term)].

tail_duplicate(N,Term)->
    tail_duplicate(N,Term,[]).
tail_duplicate(0,_,List)->
    List;
tail_duplicate(N,Term,List) when N > 0 ->
   tail_duplicate(N-1, Term, [Term|List]).


reverse([])->[];
reverse([H|T])->
    reverse(T)++[H].


tail_reverse(L)->
    tail_reverse(L,[]).
tail_reverse([], Accumulator) -> Accumulator;
tail_reverse([H|T],Accumulator)->
     tail_reverse(T, [H|Accumulator]).

sublist(_,0)->[];
sublist([],_)->[];
sublist([H|T], N) when N > 0 ->
     [H|sublist(T, N - 1)].

