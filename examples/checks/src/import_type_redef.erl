-lang([erl2, st]).
-module(import_type_redef).

-import_type(foo, [f/2]).

-import_type(bar, [f/2]).

-export([f/0]).

-spec f() -> foo:f(atom(), atom()).
f() ->
    ok.
