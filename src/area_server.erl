-module(area_server).
-export([loop/0, rpc/2, start/0, area/2]).

%server start
start() ->
     spawn(?MODULE, loop, []).

area(Pid, What) ->
    rpc(Pid, What).

%client call
rpc(Pid, Request)->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

%server call
loop() ->
    receive
        {From, {rectangle, Width, Height}}->
            From ! {self(), Width * Height},
            loop();
        {From, {circle, R}} ->
            From ! {self(), 3.14 * R * R},
            loop();
        {From, _} ->
             From ! {self(), "nothing to do"},
             loop()
    end.
