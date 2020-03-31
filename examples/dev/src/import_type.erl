-lang([erl2,st]).
-module(import_type).
-compile([warn_unused_import,warnings_as_errors]).

-export([f/1]).

-import_type(mod01, [my_pair/2]).

-spec f(my_pair(boolean(), integer())) -> integer().
f(X) ->
    case X of
        {true, N} -> N;
        {false,_ } -> 0
    end.
