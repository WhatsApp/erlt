-lang([erl2, st]).
-module(undefined_constr).

-export([f/1]).

-enum foo() :: bar{}.

%% Error: referring to an unknown constructor in an existing enum
f(foo.baz{}) ->
    true.
