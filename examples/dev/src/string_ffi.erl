-lang([erl2, ffi]).
-module(string_ffi).

-compile(export_all).

-spec length(string()) -> integer().
length(S) ->
    erlang:length(S).
