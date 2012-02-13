%% Higher Order Functions
-module(hhfns).
-compile(export_all).

one()->1.
two()->2.
add(X,Y)->
    X() + Y().

%% Invoke to see how higher order functions work

map(_,[])->[];
map(F, [H|T])->
    [F(H)|map(F,T)].


increment(X)->
    X + 1.
decrement(Y)->
    Y - 1.

%% Invoke using Closures
a()->
    Secret = "pony",
    fun()-> Secret end.
b(F)->
    "a/0's password is "++F().


%% Filtering. Keep only even numbers from a list

even(L)-> even(L,[]).
even([],Acc)->Acc;
even([H|T], Acc) when H rem 2 == 0 ->
    even(T,[H|Acc]);
even([_|T],Acc)->
    even(T,Acc).

%% Filtering keep men older than 60
old_men(L) -> old_men(L,[]).

old_men([], Acc) -> Acc;
old_men([Person = {male, Age}|People], Acc) when Age > 60 ->
    old_men(People, [Person|Acc]);
old_men([_|People], Acc) ->
    old_men(People, Acc).

%% Functions to filter lists with a defined predicate

filter(Predicate, L)->lists:reverse(filter(Predicate, L, [])).

filter(_, [], Acc) -> Acc;
filter(Predicate, [H|T], Acc)->
    case Predicate(H) of
        true -> filter(Predicate, T, [H|Acc]);
        false -> filter(Predicate, T, Acc)
    end.
