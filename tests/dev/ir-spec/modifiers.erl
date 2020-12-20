-file("dev/src/modifiers.erlt", 1).

-module(modifiers).

-unchecked([{dt, 1},
            {empty, 0},
            {ffi, 1},
            {get, 2},
            {put, 3}]).

-export([ffi/1, dt/1, empty/0, get/2, put/3]).

-export_type([map_/2]).

-type map_(_Key, _Value) :: term().

-spec ffi(integer()) -> binary().

ffi(X) -> erlang:term_to_binary(X).

-spec empty() -> map_(_Key, _Value).

empty() -> maps:empty().

-spec get(Key, map_(Key, Value)) -> Value.

get(Key, Map) -> maps:get(Key, Map).

-spec put(Key, Value, map_(Key, Value)) -> map_(Key,
                                                Value).

put(Key, Value, Map) -> maps:put(Key, Value, Map).

dt(X) -> [X, {X}, [X], erlang:term_to_binary(X)].



