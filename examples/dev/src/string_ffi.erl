-lang([erl2, ffi]).
-module(string_ffi).

-spec length(string()) -> integer().
length(S) ->
    erlang:length(S).
