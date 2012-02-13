%% Higher Order Functions
-module(hhfns).
-compile(export_all).

one()->1.

two()->2.

add(X,Y)->
    X() + Y().


map(_,[])->[];

map(F, [H|T])->
    [F(H)|map(F,T)].


increment(X)->
    X + 1.

decrement(Y)->
    Y - 1.

a()->
    Secret = "pony",
    fun()-> Secret end.

b(F)->
    "a/0's password is "++F().