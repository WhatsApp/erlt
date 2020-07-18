-lang([erl2, st]).
-module(comp).

map(F, L) ->
    [F(X) || X <- L].

filter(F, L) ->
    [X || X <- L, F(X)].

strange_perm(F, L1, L2) ->
    [{X, Y} || X <- L1, Y <- L2, F(X, Y)].

sort([Pivot|T]) ->
    sort([ X || X <- T, X < Pivot])
        ++ [Pivot]
        ++ sort([ Y || Y <- T, Y >= Pivot]);
sort([]) -> [].


sort([Pivot|T], C) ->
    sort([ X || X <- T, C(X, Pivot)], C)
        ++ [Pivot]
        ++ sort([ Y || Y <- T, not C(Y, Pivot)], C);
sort([], _C) -> [].

append(L) ->
    [X || L1 <- L, X <- L1].

flat_map(F, L) ->
    [Y || X <- L, Y <- F(X)].
