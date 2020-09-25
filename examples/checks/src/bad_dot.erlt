-lang([erl2, st]).
-module(bad_dot).

-export([]).

-spec mk_raw_atom() -> atom().
test_dot(X) ->
    % Error: the right hand side of a dot operator must be a literal name
    X.(f(X)).

f(X) ->
    list_to_atom(X).
