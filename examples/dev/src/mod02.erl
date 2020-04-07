-lang([erl2, st]).
-module(mod02).

-export([odd/1, id/1, id_caller/1, id_rec/1, even/1, mod01call/1, list_to_string/1, int_to_string/1]).

-spec odd(integer()) -> boolean().
odd(X) ->
    even(X - 1).

-spec id(X) -> X.
id(X) ->
    X.

-spec id_caller(X) -> X.
id_caller(X) ->
    id(X).

-spec id_rec(X) -> X.
id_rec(X) ->
    id_rec(X).

-spec even(integer()) -> boolean().
even(0) ->
    true;
even(X) ->
    odd(X - 1).

-spec mod01call(X) -> X.
mod01call(X) ->
    mod01:mod01F(X).

-spec list_to_string([_]) -> string().
list_to_string([]) ->
    ffi:to_string([]);
list_to_string(X) ->
    ffi:to_string(X).

-spec int_to_string(integer()) -> string().
int_to_string(0) ->
    ffi:to_string(0);
int_to_string(N) ->
    ffi:to_string(N).
