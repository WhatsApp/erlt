-lang([erl2, st]).
-module(unqualified_enum).

-export([f/1]).

-enum foo() :: bar{}.

%% Error: must use enum qualifier to refer to constructor: foo.bar{}, not bar{}
f(bar{}) ->
    true.
