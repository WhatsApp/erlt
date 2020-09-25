-lang([erlt]).
-module(unused_struct).

-export([test/2]).

-export_type([used_exported/0]).

-struct unused :: ().

-struct used_expr :: ().

-struct used_field :: (a :: integer()).

-struct used_guard :: ().

-struct used_pattern :: ().

-struct used_exported :: ().

test(#used_pattern{}, X) when X =:= #used_guard{} ->
    {X#used_field.a, #used_expr{}}.
