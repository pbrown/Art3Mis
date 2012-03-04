% Learning multiprocessing
-module(linkmon).
-compile(export_all).

myproc()->
    timer:sleep(5000),
    exit(reason).

chain(0)->
    receive
       _ -> ok
    after 2000 ->
        exit("chain dies here")
    end;
chain(N) ->
    Pid = spawn(fun() -> chain(N-1) end),
    link(Pid),
    receive
        _ -> ok
    end.


start_critic() ->
    spawn(?MODULE, critic, []).

judge(Pid, Band, Album) ->
    Pid ! {self(), {Band, Album}},
    receive
        {Pid, Criticism} -> Criticism
     after 2000 ->
        timeout
     end.


critic() ->
    receive
        {From, {"Rage against the turning machine", "Unit Testify"}} ->
            From! {self(), "They are great!"};
        {From, {"System of downtime", "Memoize"}} ->
            From! {self(), "They are not johny cash, but they are good "};
        {From, {"Johny Crash", "The token ring of fire"}} ->
            From! {self(), "Simply Incredible "};
        {From, {_Band, _Album}}->
            From! {self(), "They are terrible!"}
    end,
    critic().


start_critic2() ->
    spawn(?MODULE, restarter, []).

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic2, []),
    register(critic, Pid),
    receive
        {'EXIT', Pid, normal} ->
            ok;
        {'EXIT', Pid, shutdown} ->
            ok;
        {'EXIT', Pid, _} ->
            restarter()
    end.

judge2(Band, Album) ->
    Ref = make_ref(),
    critic ! {self(), Ref,  {Band, Album}},
    receive
        {Ref, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

critic2() ->
    receive
        {From, Ref, {"Rage against the turning machine", "Unit Testify"}} ->
            From! {Ref, "They are great!"};
        {From, Ref, {"System of downtime", "Memoize"}} ->
            From! {Ref, "They are not johny cash, but they are good "};
        {From, Ref, {"Johny Crash", "The token ring of fire"}} ->
            From! {Ref, "Simply Incredible "};
        {From, Ref, {_Band, _Album}}->
            From! {Ref, "They are terrible!"}
    end,
    critic2().
