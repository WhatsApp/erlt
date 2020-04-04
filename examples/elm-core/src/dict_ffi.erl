-lang([erl2, ffi]).
-module(dict_ffi).

-compile(export_all).

-export_type([dict/2]).

-type dict(K, V) :: #{K => V}.

-spec empty() -> dict(_, _).
empty() ->
    maps:new().

-spec get(K, dict(K, V)) -> maybe:maybe(V).
get(Key, Dict) ->
    case maps:find(Key, Dict) of
        {ok,Value} -> maybe.just{Value};
        error -> maybe.nothing{}
    end.


-spec size(dict(_,_)) -> integer().
size(Dict) ->
    maps:size(Dict).

-spec insert(K, V, dict(K, V)) -> dict(K, V).
insert(Key, Value, Dict) ->
    maps:put(Key, Value, Dict).

-spec remove(K, dict(K, V)) -> dict(K, V).
remove(Key, Dict) ->
    maps:remove(Key, Dict).

-spec fold(fun((K, V, R) -> R), R, dict(K, V)) -> R.
fold(F, Acc, Dict) ->
    maps:fold(F, Acc, Dict).

-spec union(dict(K, V), dict(K, V)) -> dict(K, V).
union(Dict1, Dict2) ->
    maps:merge(Dict1, Dict2).

-spec filter(fun((K, V) -> boolean()), dict(K, V)) -> dict(K, V).
filter(F, Dict) ->
    maps:filter(F, Dict).

-spec map(fun((K, A) -> B), dict(K, A)) -> dict(K, B).
map(F, Dict) ->
    maps:map(F, Dict).

-spec keys(dict(K, _)) -> list(K).
keys(Dict) ->
    maps:keys(Dict).

-spec values(dict(_, V)) -> list(V).
values(Dict) ->
    maps:values(Dict).

-spec to_list(dict(K, V)) -> list({K, V}).
to_list(Dict) ->
    maps:to_list(Dict).

-spec from_list(list({K, V})) -> dict(K, V).
from_list(List) ->
    maps:from_list(List).
