%% Event Server
-module(evserv).
-compile(export_all).

% List of spawned events and subscribing clients
-record(state, {events, clients}).

-record(event, {name="", description="", pid, timeout={{1970,1,1},{0,0,0}}}).

start() ->
    register(?MODULE, Pid=spawn(?MODULE, init, [])),
    Pid.

start_link()->
    register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
    Pid.

init() ->
    loop(#state{events=orddict:new(), clients=orddict:new()}).

loop(S=#state{}) ->
    receive
       {Pid, MsgRef, {subscribe, Client}} ->
            Ref = erlang:monitor(process, Client),
            io:format("Reference to Client is ~p~n", [Ref]),
            NewClients = orddict:store(Ref, Client, S#state.clients),
            io:format("Subscribing clients are ~p~n", [NewClients]),
            Pid ! {MsgRef, ok},
            loop(S#state{clients=NewClients});
        Unknown ->
                io:format("Unknown message is used: ~p~n", [Unknown]),
                loop(S)
    end.