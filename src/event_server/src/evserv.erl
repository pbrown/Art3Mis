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
                subscribe(Sock, PathString, QueryString, Params, Fragment, Headers, Body, Pid);
            "/add-event" ->
                add_event(Sock, PathString, QueryString, Params, Fragment, Headers, Body, Pid);
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


subscribe(Sock, PathString, QueryString, Params, Fragment, Headers, Body, Pid)->
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


add_event(Sock, PathString, QueryString, Params, Fragment, Headers, Body, Pid) ->
   Ref = make_ref(),
   gen_tcp:send(Sock, "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=UTF-8\r\nConnection: close\r\n\r\nTemporarily event added!\r\n\r\n").

create_record(json, Body)->
    Accumulator = #event{},
    JsonKV = ejson:json(Body),
    io:format("JsonKV ~p~n", [JsonKV]),
    Foo=lists:foreach(fun(H) ->
        io:format("H value is ~p~n", [H]),
        case H of
         {"name", Name} ->
                    io:format("Did I come here ~p~n",[Name]),
                    MyFoo = Accumulator#event{name = Name},
                    io:format("Accu ~p~n", [MyFoo]);
         {"description", Description} ->
                    Accumulator#event{description = Description}
         end end, JsonKV),

     io:format("Accumulator ~p~n", [Foo]).


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
            io:format("Adding Event ~p~n", [Name]),
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

% Extract Body of a Post Element  and return back as a list of lists
parse(json, Body) ->
    io:format("Body is ~p~n", [Body]),
    Tokens = string:tokens(Body, "{},:"),
    io:format("Tokens are ~p~n", [Tokens]),
    MyTokens = lists:flatmap(fun(X)->string:substr(X, 2, 4) end, Tokens),
    io:format("My Tokens are ~p~n", [MyTokens]),
    Acc = parseTokens(Tokens),
    io:format("Accumulated values ~p~n", [Acc]).

parseTokens(L)->
    parseTokens(L, Event=#event{}).

parseTokens([], Accumulator) -> Accumulator;

parseTokens([H|T],Accumulator) ->
     Token = lists:nth(1,H),
     io:format("Token being evaluated now ~p~n", [Token]),
     case Token of
         "name" ->
                    Accumulator#event{name = lists:nth(1, H)};
         "description" ->
                    Accumulator#event{description = lists:nth(1, H)};
          _ ->
                io:format("Dropping element ~p~n", [H])
     end,
     parseTokens(T, Accumulator).

