-lang([erl2]).
-module(remote_constr_wrong_arity).

-export([f/1]).

%% Error: referring to an unknown constructor in an existing enum
f(define_enum.foo.bar{A}) ->
    A.
