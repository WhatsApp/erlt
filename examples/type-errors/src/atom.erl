-lang([erl2, st]).
-module(atom).

-export([mk_raw_atom/0]).

-spec mk_raw_atom() -> atom().
mk_raw_atom() ->
    % Error: raw atoms are not allowed in st
    atom.
