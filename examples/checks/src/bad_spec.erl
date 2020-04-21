-lang([erl2, st]).
-module(bad_spec).

-export([mk_raw_atom/1]).

% Error: `when`s are not allowed in specs
-spec mk_raw_atom(A) -> A when A :: term().
mk_raw_atom(X) ->
    X.
