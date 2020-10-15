-file("dev/src/modifiers.erlt", 1).

-module(modifiers).

-export([ffi/1, dt/1, empty/0, get/2, put/3]).

-type map(_Key, _Value) :: term().

-spec ffi(integer()) -> binary().

ffi(X) -> erlang:term_to_binary(X).

-spec empty() -> map(_Key, _Value).

empty() -> maps:empty().

-spec get(Key, map(Key, Value)) -> Value.

get(Key, Map) -> maps:get(Key, Map).

-spec put(Key, Value, map(Key, Value)) -> map(Key,
                                              Value).

put(Key, Value, Map) -> maps:put(Key, Value, Map).

dt(X) -> [X, {X}, [X], erlang:term_to_binary(X)].



