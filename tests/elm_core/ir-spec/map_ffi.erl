-file("elm_core/src/map_ffi.erlt", 1).

-module(map_ffi).

-export_type([map/2]).

-export([empty/0,
         get/2,
         size/1,
         insert/3,
         remove/2,
         fold/3,
         union/2,
         filter/2,
         map/2]).

-export([keys/1, values/1, to_list/1, from_list/1]).

-import_type({maybe, [{maybe, 1}]}).

-type map(_K, _V) :: term().

-spec empty() -> map(_K, _V).

empty() -> maps:new().

-spec get(K, map(K, V)) -> maybe:maybe(V).

get(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> {'$#maybe:maybe.just', Value};
        error -> {'$#maybe:maybe.nothing'}
    end.

-spec size(map(_K, _V)) -> integer().

size(Map) -> maps:size(Map).

-spec insert(K, V, map(K, V)) -> map(K, V).

insert(Key, Value, Map) -> maps:put(Key, Value, Map).

-spec remove(K, map(K, V)) -> map(K, V).

remove(Key, Map) -> maps:remove(Key, Map).

-spec fold(fun((K, V, R) -> R), R, map(K, V)) -> R.

fold(F, Acc, Map) -> maps:fold(F, Acc, Map).

-spec union(map(K, V), map(K, V)) -> map(K, V).

union(Map1, Map2) -> maps:merge(Map1, Map2).

-spec filter(fun((K, V) -> boolean()),
             map(K, V)) -> map(K, V).

filter(F, Map) -> maps:filter(F, Map).

-spec map(fun((K, A) -> B), map(K, A)) -> map(K, B).

map(F, Map) -> maps:map(F, Map).

-spec keys(map(K, _V)) -> [K].

keys(Map) -> maps:keys(Map).

-spec values(map(_K, V)) -> [V].

values(Map) -> maps:values(Map).

-spec to_list(map(K, V)) -> [{K, V}].

to_list(Map) -> maps:to_list(Map).

-spec from_list([{K, V}]) -> map(K, V).

from_list(List) -> maps:from_list(List).


