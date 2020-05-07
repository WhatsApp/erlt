-lang([erl2, st]).
-module(unquoted_atom).

-export([some_atoms/0]).

-spec some_atoms() -> atom().
some_atoms() ->
    %% predefined symbols for standard atoms - no quotes needed
    {true,
     false,
     ok,
     undefined,
     error,
     exit,
     throw,
     %% Error: unquoted arbitrary atoms are considered unknown symbols
     bebop
    }.
