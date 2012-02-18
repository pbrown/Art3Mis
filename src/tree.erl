-module(tree).
-compile(export_all).

%% Binary Tree Representation
insert(Key, Val, {node, 'nil'}) ->
    {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
    {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
    {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
    {node, {Key, Val, Smaller, Larger}}.

empty()->{node, 'nil'}.

%% Find a given key in a Binary tree recursively

lookup(_, {node, 'nil'}) ->
    undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
    {ok, Val};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
    lookup(Key, Smaller);
lookup(Key, {node, {_, _, _, Larger}}) ->
    lookup(Key, Larger).


 %% Find a given value in a binary truee
 has_value(_, {node, 'nil'})->
    false;
 has_value(Value, {node, {_, Value, _, _}})->
    true;
 has_value(Value, {node, {_, _, Left, Right}})->
    case has_value(Value, Left) of
        true-> true;
        false->has_value(Value, Right)
    end.

%Using throws to break an execution of a function when the value is found
has_myvalue(Value, Tree)->
    try has_value1(Value, Tree) of
        false->false
    catch
        true->true
    end.

has_value1(_, {node,'nil'})->
    false;
has_value1(Val, {node, {_, Val, _, _}})->
    throw(true);
has_value1(Val, {node, {_,_,Left,Right}})->
    has_value1(Val, Left),
    has_value1(Val, Right).


