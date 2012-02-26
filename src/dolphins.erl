-module(dolphins).
-compile(export_all).

dolphin1()->
	receive
		do_a_flip->
			io:format("How about no?~n");
		fish ->
			io:format("So long and thanks for the fish ~n");
		_ -> 
			io:format("Heh, we are smarter than you hunmans. ~n")


	end.


dolphin2()->
	receive
		{From, do_a_flip}->
			From ! "How about no?";
		{From, fish}->
			From ! "So long and thanks for the fish!";
		_ ->
			io:format("Heh, we are smarter than you humans. ~n")
	end.


dolphin3()->
    receive
        {From, do_a_flip} ->
            From ! "How about no",
            dolphin3();
        {From, fish} ->
            From ! "So long and thanks for the fish!",
            dolphin3();
        _ ->
            io:format("Heh, we are smarter than you humans. ~n"),
            dolphin3()
    end.
