-lang([erl2, st]).
-module(undefined_enum).

-export([f/1]).

%% Error: referring to an unknown enum type in the current module
f(bar.baz{}) ->
    true.
