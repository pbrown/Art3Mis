-module(exceptions).
-compile(export_all).

throws(F) ->
   try F() of 
	_ -> ok
   catch 
	Throw -> {throw, caught, Throw}
   end.

errors(F) -> 
   try F() of 
	_ -> ok 
   catch
       error:Error -> {error, caught, Error}
   end.

exits(F) ->
   try F() of 
	_ -> ok
   catch
	exit:Exit -> {exit, caught, Exit}
   end.

sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).
 
black_knight(Attack) when is_function(Attack, 0) ->
	try Attack() of
		_ -> "None shall pass."
	catch
		throw:slice -> "It is but a scratch.";
		error:cut_arm -> "I've had worse.";
		exit:cut_leg -> "Come on you pansy!";
		_:_ -> "Just a flesh wound."
	end.

talk()->"blah blah".


whoa()->
    try
        talk(),
        _Knight= "None shall pass",
        _Doubles = [ N*2 || N <- lists:seq(1,100)],
        throw(up),
        _WillReturnThis = tequila
     catch
        Exception:Reason->{caught, Exception, Reason}
     end.

%% Tree Traversal (find a value in a binary tree)
%% Tree is in the form of {node, {Key, Value, NodeLeft, NodeRight}} or {node, 'nil'}

has_value(_, {node, nil})->
    false;
has_value(Val, {node, {_, Val, _, _}})->
    true;
has_value(Val, {node, {_ ,_ ,Left,Right}})->
    case has_value(Val, Left) of
        true ->true;
        false -> has_value(Val, Right)
     end.
