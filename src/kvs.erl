-module(kvs).
-compile(export_all).

%initialize the name server, bind process to NameServer
start()->
    register(kvs, spawn(?MODULE, loop, [])).


store(Key, Value) -> rpc({store, Key, Value}).

lookup(Key) -> rpc({lookup, Key}).


rpc(Q) ->
    kvs ! {self(), Q},
    receive
        {kvs, Reply} -> Reply
    end.

loop() ->
    receive
        {From, {store, Key, Value}}->
            put(Key, {ok, Value}),
            From ! {kvs, true},
            loop();
        {From, {lookup, Key}}->
            From ! {kvs, get(Key)},
            loop();
        _ ->
            io:format("I have no idea what to do"),
            loop()
    end.

