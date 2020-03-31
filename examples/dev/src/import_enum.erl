-lang([erl2]).
-module(import_enum).
-compile([warn_unused_import,warnings_as_errors]).

-export([swap/1]).

-import_type(mod03, [pair/2]).

-spec swap(pair(A, B)) -> pair(A, B).

swap(pair.pair{A, B}) -> pair.pair{B, A}.
