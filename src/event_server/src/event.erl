-module(event).
-compile(export_all).
-record(state, {server, name="", to_go=0}).

start(EventName, DateTime) ->
	spawn(?MODULE, init, [self(), EventName, DateTime]).

start_link(EventName, DateTime) ->
	spawn_link(?MODULE, init, [self(), EventName, DateTime]).

init(Server, EventName, DateTime) ->
	loop(#state{server=Server, name=EventName, to_go=time_to_go(DateTime)}).

cancel(Pid) -> 
	Ref = erlang:monitor(process, Pid),
	Pid ! {self(), Ref, cancel},
	receive
		{Ref, ok} ->
			erlang:demonitor(Ref, [flush]),
			ok;
		{'DOWN', Ref, process, Pid, _Reason} ->
			ok
	end.

% Run each event as a process that expires that dies when its time to send a reminder
% If a cancel message is received, then .....
loop(S = #state{server=Server, to_go=[T|Next]})->
	receive
		{Server, Ref, cancel} ->
			Server ! {Ref, cancel} 
	after T * 1000 ->
		if Next =:= [] -> 
			Server ! {done, S#state.name};
		   Next =/= [] ->
			loop(S#state{to_go=Next})
		end
	end.	

%% Dealing with erlang limits of 50 days of milliseconds.
normalize(N)->
	Limit = 49 * 24 * 60 * 60,
	[N rem Limit | lists:duplicate(N div Limit, Limit)].

time_to_go(Timeout={{_,_,_}, {_,_,_}}) ->
	Now = calendar:local_time(),
	io:format("Now: ~p", [Now]),
	ToGo = calendar:datetime_to_gregorian_seconds(Timeout) - calendar:datetime_to_gregorian_seconds(Now),
	io:format("Time to go: ~p", [ToGo]),
	Secs = if ToGo > 0 -> ToGo;
		  ToGo =<0 -> 0
	       end,
	normalize(Secs).


