-lang([erl2, st]).
-module(undefined_remote_enum).

-export([f/1]).

%% Error: referring to an unknown enum type from a remote module
f(foo.bar.baz{}) ->
    true.