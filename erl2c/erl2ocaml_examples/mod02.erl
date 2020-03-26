-module(mod02).
-compile(export_all).

odd(X) ->
    even(X).

id(X) ->
    X.

id_caller(X) ->
    id(X).

id_rec(X) ->
    id_rec(X).

even(X) ->
    odd(X).

mod01call(X) ->
    mod01:mod01F(X).

list_to_string([]) ->
    ffi:to_string([]);
list_to_string(X) ->
    ffi:to_string(X).

int_to_string(0) ->
    ffi:to_string(0);
int_to_string(N) ->
    ffi:to_string(N).
