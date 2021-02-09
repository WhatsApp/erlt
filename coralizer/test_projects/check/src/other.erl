-module(other).

-compile([export_all, nowarn_export_all]).

% occurrence typing
-spec clauses(any()) ->
    atom() | number().
clauses(X) ->
    case X of
        ok -> X;
        1 -> X
    end.

-spec inter
    (ok) -> nok;
    (nok) -> ok.
inter(ok) -> nok;
inter(nok) -> ok.

-spec slice_inter1(ok) -> nok.
slice_inter1(ok) -> inter(ok).

-spec slice_inter2(ok) -> nok.
slice_inter2(ok) ->
    other:inter(ok).

-spec get_inter1() ->
    any().
get_inter1() ->
    fun inter/1.

-spec get_inter2() ->
    any().
get_inter2() ->
    fun other:inter/1.
