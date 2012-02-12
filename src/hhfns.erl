%% Higher Order Functions
-module(hhfns).
-compile(export_all).

one()->1.

two()->2.

add(X,Y)->
    X() + Y().


operate(_,[])->[];

operate(F, [H|T])->
    [F(H)|operate(F,T)].


increment(X)->
    X + 1.

decrement(Y)->
    Y - 1.