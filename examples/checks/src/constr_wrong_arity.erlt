-lang([erl2, st]).
-module(constr_wrong_arity).

-export([f/1]).

-enum foo(T) :: bar{T}.

%% Error: wrong number of arguments to the constructor
f(foo.bar{A, B}) ->
    true.
