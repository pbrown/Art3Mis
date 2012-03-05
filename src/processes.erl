-module(processes).
-export([max/1, wait/0, for/3]).

max(N) ->
    Max = erlang:system_info(process_limit),

    io:format("Maximum allowed processes: ~p~n", [Max]),
    statistics(runtime),
    statistics(wall_clock),
    L = for(1, N, fun() -> spawn(fun() -> wait() end) end),
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    lists:foreach(fun(Pid) -> Pid ! die end, L),
    Val1 = Time1 * 1000 / N,
    Val2 = Time2 * 1000 / N,
    io:format("cpu time: ~p~n", [Val1]),
    io:format("wall-clock time: ~p~n", [Val2]).



wait() ->
    receive
        die -> void
    end.

for(N, N, F) -> [F()];
for(I, N, F) -> [F()|for(I+1, N, F)].