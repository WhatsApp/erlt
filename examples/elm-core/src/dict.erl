-lang([erl2, st]).
-module(dict).

-compile(export_all).

-type dict(K, V) :: dict_ffi:dict(K, V).

-spec empty() -> dict(_K,_V).
empty() ->
    dict_ffi:empty().

-spec get(K, dict(K, V)) -> maybe:maybe(V).
get(Key, Dict) -> dict_ffi:get(Key, Dict).

-spec member(K, dict(K, _V)) -> boolean().
member(Key, Dict) ->
    case get(Key, Dict) of
        maybe.just{_} -> true;
        maybe.nothing{} -> false
    end.

-spec size(dict(_K,_V)) -> integer().
size(Dict) ->
    dict_ffi:size(Dict).

-spec is_empty(dict(_K, _V)) -> boolean().
is_empty(Dict) ->
    dict:size(Dict) =:= 0.

-spec insert(K, V, dict(K, V)) -> dict(K, V).
insert(Key, Value, Dict) ->
    dict_ffi:insert(Key, Value, Dict).

-spec remove(K, dict(K, V)) -> dict(K, V).
remove(Key, Dict) ->
    dict_ffi:remove(Key, Dict).

-spec update(K, fun((maybe:maybe(V)) -> maybe:maybe(V)), dict(K, V)) -> dict(K, V).
update(Key, Alter, Dict) ->
    case Alter(get(Key, Dict)) of
        maybe.just{Value} -> insert(Key, Value, Dict);
        maybe.nothing{} -> remove(Key, Dict)
    end.

-spec singleton(K, V) -> dict(K, V).
singleton(Key, Value) ->
    insert(Key, Value, empty()).

%% COMBINE

-spec union(dict(K, V), dict(K, V)) -> dict(K, V).
union(Dict1, Dict2) ->
    dict_ffi:union(Dict1, Dict2).

-spec intersect(dict(K, V), dict(K, V)) -> dict(K, V).
intersect(Dict1, Dict2) ->
    filter(fun(K,_) -> member(K, Dict2) end, Dict1).

-spec diff(dict(K, V), dict(K, V)) -> dict(K, V).
diff(Dict1, Dict2) ->
    filter(fun(K,_) -> not member(K, Dict2) end, Dict1).

%% TRANSFORM

-spec fold(fun((K, V, R) -> R), R, dict(K, V)) -> R.
fold(F, Acc, Dict) ->
    dict_ffi:fold(F, Acc, Dict).

-spec map(fun((K, A) -> B), dict(K, A)) -> dict(K, B).
map(F, Dict) ->
    dict_ffi:map(F, Dict).

-spec filter(fun((K, V) -> boolean()), dict(K, V)) -> dict(K, V).
filter(F, Dict) ->
    dict_ffi:filter(F, Dict).

-spec partition(fun((K, V) -> boolean()), dict(K, V)) -> {dict(K, V), dict(K, V)}.
partition(F, Dict) ->
    Add =
        fun(K, V, {D1, D2}) ->
            case F(K, V) of
                true -> {insert(K, V, D1), D2};
                false -> {D1, insert(K, V, D2)}
            end
        end,
    fold(Add, {empty(), empty()}, Dict).

%% LISTS

-spec keys(dict(K, _V)) -> list(K).
keys(Dict) ->
    dict_ffi:keys(Dict).

-spec values(dict(_K, V)) -> list(V).
values(Dict) ->
    dict_ffi:values(Dict).

-spec to_list(dict(K, V)) -> list({K, V}).
to_list(Dict) ->
    dict_ffi:to_list(Dict).

-spec from_list(list({K, V})) -> dict(K, V).
from_list(List) ->
    dict_ffi:from_list(List).
