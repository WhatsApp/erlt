-module(test).

-spec clauses(any()) ->
    atom() | number().
clauses(X) ->
    case X of
        1 -> 1;
        1 -> ok
    end.
