-lang([erl2]).
-module(undefined_remote_constr).

-export([f/1]).

%% Error: referring to an unknown constructor in an existing enum
f(define_enum.foo.baz{}) ->
    true.