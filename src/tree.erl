-module(tree).
-compile(export_all).

insert(Key, Value, {node, 'nil'})->
	{node, {Key, Value, {node, 'nil'}, {node, 'nil'}}}.

empty()->{node, 'nil'}.
