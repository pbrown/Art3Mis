-module(multiproc).
-compile(export_all).

<<<<<<< HEAD
important()->
	receive
		{Priority, Message} when Priority > 10 ->
			[Message|important()]
	after 0 ->
		normal()
	end.

normal()->
	receive
		{_, Message} ->
			[Message|normal()]
	after 0 ->
		[]
	end.
=======
sleep(T)->
    receive
    after T -> ok
    end.

flush() ->
    receive
        _ -> flush()
    after 0 ->
        ok
    end.

important() ->
    receive
        {Priority, Message} when Priority > 10 ->
            [Message| important()]
    after 0 ->
        normal()
    end.

normal()->
    receive
        {_, Message} ->
            [Message | normal()]
    after 0 ->
         []
    end.
>>>>>>> 84bd6b6dc804a40ece42bd32a4a9140a897b5b62
