-lang(st).
-module(erl2).

%% testing that different constructs are mappable

multi_clause_local_fun() ->
    fun (false) -> false; (true) -> true end.

multi_clause_named_local_fun() ->
    fun Loc (false) -> false;
        Loc (true) -> true end.
