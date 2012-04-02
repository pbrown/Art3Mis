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

terminate() ->
    ?MODULE ! shutdown.

init() ->
    loop(#state{events=orddict:new(), clients=orddict:new()}).

create_get_function() ->
    fun(Sock, PathString, QueryString, Params, Fragment, Headers, Body, Pid)->
            greet(Sock, Pid)
    end.

create_post_function() ->
    fun(Sock, PathString, QueryString, Params, Fragment, Headers, Body, Pid)->
          io:format("PathString: ~p~n QueryString: ~p~n Params: ~p~n", [PathString, QueryString, Params]),
          case PathString of
            "/subscribe" ->
                subscribe(Sock, Pid);
            "/add-event" ->
                add_event(Sock, Body, Pid);
            _ ->
             gen_tcp:send(Sock, "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=UTF-8\r\nConnection: close\r\n\r\nUnrecognized QueryString!\r\n\r\n")
          end
    end.

greet(Sock, Pid) ->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    ?MODULE ! {self(), Ref, {greet, Pid}},
    receive
        {Ref, ok} ->
            gen_tcp:send(Sock, "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=UTF-8\r\nConnection: close\r\n\r\nWelcome to Event Server!\r\n\r\n"),
            {ok, Ref};
        {DOWN, Ref, process, _Pid, Reason}->
            {error, reason}
    after(5000) ->
        {error, timeout}
    end.


subscribe(Sock, Pid)->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    ?MODULE ! {self(), Ref, {subscribe, Pid}},
    receive
        {Ref, ok} ->
            gen_tcp:send(Sock, "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=UTF-8\r\nConnection: close\r\n\r\nClient Subscribed!\r\n\r\n"),
            {ok, Ref};
        {DOWN, Ref, process, _Pid, Reason}->
            {error, reason}
    after(5000) ->
        {error, timeout}
    end.


add_event(Sock, Body, Pid) ->
   Ref = make_ref(),
   Event = create_event(json, Body),
   io:format("Parsed Event ~p~n", [Event]),
   ?MODULE ! {self(), Ref, {add, Event#event.name, Event#event.description, Event#event.timeout}},
   receive
        {Ref, ok} ->
            gen_tcp:send(Sock, "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=UTF-8\r\nConnection: close\r\n\r\nEvent added!\r\n\r\n"),
            {ok, Ref};
        {DOWN, Ref, process, _Pid, Reason}->
            {error, reason}
   after(5000) ->
        {error, timeout}
   end.


loop(S=#state{}) ->
    io:format("Registered Clients ~p~n", [S#state.clients]),
    io:format("Registered Events ~p~n", [S#state.events]),
    receive
        {Pid, MsgRef, {greet, Client}} ->
            io:format("Pinged to Greet ~n"),
            Pid ! {MsgRef, ok},
            loop(S);
        {Pid, MsgRef, {subscribe, Client}} ->
            Ref = erlang:monitor(process, Client),
            NewClients = orddict:store(Ref, Client, S#state.clients),
            Pid ! {MsgRef, ok},
            loop(S#state{clients=NewClients});
        {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
            io:format("Event ~p to be fired at ~p~n", [Name, TimeOut]),
            case valid_datetime(TimeOut) of
                true ->
                    EventPid = event:start_link(Name, TimeOut),
                    NewEvents = orddict:store(Name,
                                              #event{name=Name,
                                                     description=Description,
                                                     pid=EventPid,
                                                     timeout=TimeOut},
                                              S#state.events),
                    Pid ! {MsgRef, ok},
                    loop(S#state{events=NewEvents});
                false ->
                    io:format("Error ~n"),
                    Pid ! {MsgRef, {error, bad_timeout}},
                    loop(S)
            end;
        {Pid, MsgRef, {cancel, Name}} ->
            io:format("Canceling Event ~p~n", [Name]),
            Events = case orddict:find(Name, S#state.events) of
                         {ok, E} ->
                             event:cancel(E#event.pid),
                             orddict:erase(Name, S#state.events);
                         error ->
                             S#state.events
                     end,
            Pid ! {MsgRef, ok},
            loop(S#state{events=Events});
        {done, Name} ->
            io:format("Event fired ~p~n", [Name]),
            case orddict:find(Name, S#state.events) of
                {ok, E} ->
                    send_to_clients({done, E#event.name, E#event.description},
                                    S#state.clients),
                    NewEvents = orddict:erase(Name, S#state.events),
                    loop(S#state{events=NewEvents});
                error ->
                    %% This may happen if we cancel an event and
                    %% it fires at the same time
                    loop(S)
            end;
        shutdown ->
            exit(shutdown);
        {'DOWN', Ref, process, _Pid, _Reason} ->
            loop(S#state{clients=orddict:erase(Ref, S#state.clients)});
        code_change ->
            ?MODULE:loop(S);
        Unknown ->
            io:format("Unknown message: ~p~n",[Unknown]),
            loop(S)
    end.


%%% Internal Functions
send_to_clients(Msg, ClientDict) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

% Valid date time format    {{YYYY, MM, DD,}, {H,M,S}}
valid_datetime({Date,Time}) ->
    io:format("Did I come here?"),
    try
        io:format("Date received: ~p ~p ~n", [Date, Time]),
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause ->
            false
    end;
valid_datetime(_) ->
    false.

valid_time({H,M,S}) -> valid_time(H,M,S).
valid_time(H,M,S) when H >= 0, H < 24,
                       M >= 0, M < 60,
                       S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.

% Utility Functions for creating an event from a body of a post

create_event(json, Body)->
    JsonKV = ejson:json(Body),
    Event = parseList(JsonKV, #event{}),
    Event.

parseList([], Accumulator)->
    Accumulator;

parseList([Next|Tail], Accumulator) ->
    case Next of
        {"name", Name} ->
                parseList(Tail, Accumulator#event{name = Name});
        {"description", Description} ->
                parseList(Tail, Accumulator#event{description = Description});
        {"timeout", TimeOut} ->
                parseList(Tail, Accumulator#event{timeout = TimeOut});
        {"pid", Pid} ->
		parseList(Tail, Accumulator#event{pid = Pid});
	_ ->
                parseList(Tail, Accumulator)
    end.

