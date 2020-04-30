-lang([erl2]).
-module('import_enum').
-compile([warn_unused_import,warnings_as_errors]).

-export([p/2]).

-import_type(mod03, [pair/2]).

-spec p(A,B) -> pair(A, B).
p(A,B) ->
    pair.pair{A, B}.
