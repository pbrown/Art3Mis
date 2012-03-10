-module(lib_misc).
-compile(export_all).

on_exit(Pid, Fun)->
    spawn(fun() ->
        process_flag(trap_exit, true),
        link(Pid),
        receive
           {'EXIT', Pid, Why} ->
                Fun(Why)
        end

    end ).



do_something()->



