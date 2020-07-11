-lang([erl2, st]).

-module(etf17).

-export([
    binary_star/2,
    binary_div/2,
    binary_rem/2,
    binary_band/2,
    binary_and/2,
    binary_plus/2,
    binary_minus/2,
    binary_bor/2,
    binary_bxor/2,
    binary_bsl/2,
    binary_bsr/2,
    binary_or/2,
    binary_xor/2,
    binary_orelse/2,
    binary_andalso/2,
    list_plus/2,
    list_minus/2,
    comp1/2,
    comp2/2,
    comp3/2,
    comp4/2,
    comp5/2,
    comp6/2,
    comp7/2,
    comp8/2
]).

%% Binary operations
-spec binary_star(integer(), integer()) -> integer().
binary_star(X, Y) ->
    X * Y.

-spec binary_div(integer(), integer()) -> integer().
binary_div(X, Y) ->
    X div Y.

-spec binary_rem(integer(), integer()) -> integer().
binary_rem(X, Y) ->
    X rem Y.

-spec binary_band(integer(), integer()) -> integer().
binary_band(X, Y) ->
    X band Y.

-spec binary_and(boolean(), boolean()) -> boolean().
binary_and(X, Y) ->
    X and Y.

-spec binary_plus(integer(), integer()) -> integer().
binary_plus(X, Y) ->
    X + Y.

-spec binary_minus(integer(), integer()) -> integer().
binary_minus(X, Y) ->
    X - Y.

-spec binary_bor(integer(), integer()) -> integer().
binary_bor(X, Y) ->
    X bor Y.

-spec binary_bxor(integer(), integer()) -> integer().
binary_bxor(X, Y) ->
    X bxor Y.

-spec binary_bsl(integer(), integer()) -> integer().
binary_bsl(X, Y) ->
    X bsl Y.

-spec binary_bsr(integer(), integer()) -> integer().
binary_bsr(X, Y) ->
    X bsr Y.

-spec binary_or(boolean(), boolean()) -> boolean().
binary_or(X, Y) ->
    X or Y.

-spec binary_xor(boolean(), boolean()) -> boolean().
binary_xor(X, Y) ->
    X xor Y.

-spec binary_orelse(boolean(), boolean()) -> boolean().
binary_orelse(X, Y) ->
    X orelse Y.

-spec binary_andalso(boolean(), boolean()) -> boolean().
binary_andalso(X, Y) ->
    X andalso Y.

-spec list_plus([A], [A]) -> [A].
list_plus(X, Y) ->
    X ++ Y.

-spec list_minus([A], [A]) -> [A].
list_minus(X, Y) ->
    X -- Y.

-spec comp1(A, A) -> boolean().
comp1(X, Y) ->
    X == Y.

-spec comp2(A, A) -> boolean().
comp2(X, Y) ->
    X /= Y.

-spec comp3(A, A) -> boolean().
comp3(X, Y) ->
    X =< Y.

-spec comp4(A, A) -> boolean().
comp4(X, Y) ->
    X < Y.

-spec comp5(A, A) -> boolean().
comp5(X, Y) ->
    X >= Y.

-spec comp6(A, A) -> boolean().
comp6(X, Y) ->
    X > Y.

-spec comp7(A, A) -> boolean().
comp7(X, Y) ->
    X =:= Y.

-spec comp8(A, A) -> boolean().
comp8(X, Y) ->
    X =/= Y.
