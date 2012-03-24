-module(ejson).
-export([json/1]).
-record(json, {list=[], raw=[]}).
-record(jsonkv, {value=[], raw=[]}).


% handles values that are quoted (this one ends the quote)
json_value_quoted(Value, [$" | T]) ->
  #jsonkv{value=Value, raw=T};

json_value_quoted(Value, [Next | T]) ->
  json_value_quoted(Value ++ [Next], T).

% returns JSON Key Values with remaining JSON
json_value(Value, RawJSON) ->
  [Next | T] = RawJSON,
  case Next of
    $: -> throw('unexpected token');
    ${ -> J = json(RawJSON),                                  % recurse to get list
            #jsonkv{value=J#json.list, raw=J#json.raw};
    $, -> #jsonkv{value=string:strip(Value), raw=RawJSON};    % terminator, return
    $} -> #jsonkv{value=string:strip(Value), raw=RawJSON};    % terminator, return
    $" -> json_value_quoted(Value, T);                        % run to next quote,exit
    _ -> json_value(Value ++ [Next], T)                       % recurse
  end.


% parses the Key Value pairs (KVPs) based on , & } delimiters
json(JSON, Key) ->
  [Next | T] = JSON#json.raw,
  case {Next, T} of
    {$", _} -> json(JSON#json{raw=T}, Key);        % ignore
    {${, _} -> json(#json{raw=T}, []);             % start new hash
    {$,, _} -> json(JSON#json{raw=T}, []);         % add new value
    {$:, _} -> KV = json_value([], T),  % get value for key
            List = lists:merge(JSON#json.list, [{string:strip(Key), KV#jsonkv.value}]),
            json(#json{list=List, raw=KV#jsonkv.raw}, []);  % add new KVP
    {$}, []} -> JSON#json.list;                    %DONE!
    {$}, _} -> JSON#json{raw=T};                   %List parse, but more remains!
    {_, _} -> json(JSON#json{raw=T}, Key ++ [Next])  % add to key
  end.
% entry point

json(RawJSON) ->
  json(#json{raw=RawJSON}, []).
	
