-lang(st).
-module(aliases).
-type pair(A, B) :: {A, B}.

-spec mk_pair(A) -> pair(A, B).
mk_pair(X) ->
    {X, X}.
