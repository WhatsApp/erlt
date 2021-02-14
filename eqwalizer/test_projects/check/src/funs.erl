-module(funs).
-compile([export_all]).

-spec test_01_pos() -> fun((number(), number()) -> number()).
test_01_pos() ->
    Mult = fun (X, Y) -> X * Y end,
    Mult.


-spec test_02_neg() -> pid().
test_02_neg() ->
    F = fun (X) -> nok end,
    F([]).

-spec test_03_neg() -> pid().
test_03_neg() ->
    Mult = fun (X, Y) -> X * Y end,
    Mult(4, 5).

-spec test_04_pos(number()) -> number().
test_04_pos(X) ->
    F = fun (X) -> other(X) end,
    F(3).

-spec test_05_pos(number()) -> number().
test_05_pos(X) ->
    F = fun () -> other(X) end,
    F().

-spec test_06_pos(number()) -> number().
test_06_pos(X) ->
    Sq = fun (Y) -> Y * Y end,
    apply_fun(Sq, 2).

-spec test_07_neg(number()) -> number().
test_07_neg(X) ->
    Sq = fun (Y) -> Y * Y end,
    apply_fun(Sq, an_atom).

-spec test_08_neg(number()) -> pid().
test_08_neg(X) ->
    NumToBool = fun (Y) -> Y * Y, true end,
    AnyToAnyToOk = fun(Z) ->
                       fun (B) ->
                         ok
                       end
                     end,
    B = NumToBool(X),
    AnyToOk = AnyToAnyToOk(B),
    AnyToOk(true).

-spec test_09_neg(atom()) -> pid().
test_09_neg(X) ->
    F = fun (X) -> X * 2 end,
    X,
    F(3).

-spec test_10_neg(atom()) -> pid().
test_10_neg(X) ->
    F = fun (X) -> X * 2 end,
    F(3),
    X.

-spec test_11_neg(atom()) -> pid().
test_11_neg(X) ->
    F = fun (X) -> X * 2 end,
    apply_fun(F, ok).

% We don't yet support calls with union types
% but easily could.
-spec test_12_neg(boolean()) -> pid().
test_12_neg(B) ->
    F = case B of
        true -> fun(_X) -> ok end;
        false -> fun(_X) -> error end
    end,
    F(2).

-spec test_13_neg() -> number().
test_13_neg() ->
    F = fun (_X, _Y) -> ok end,
    reduce_nums(F, [1, 2, 3]).

-spec test_14_pos() -> number().
test_14_pos() ->
    F = fun (X, _Y) -> X end,
    reduce_nums(F, [1, 2, 3]).

-spec reduce_nums(
    fun((number(), number()) -> number()),
    [number()]
) -> number().
reduce_nums(_, _) -> 0.

-spec test_15_neg() -> any().
test_15_neg() ->
    F = fun (X, Y) -> X * Y end,
    F(2).

-spec test_16_neg() -> pid().
test_16_neg() ->
  rec(5).

rec(0) -> [];
rec(1) -> [];
rec(X) -> [X|rec(X - 1)].

% TODO: arity check for higher-order funs

apply_fun(F, Arg) ->
    case F of
      _ -> F(Arg)
    end.

other(Z) -> other_other(Z).

other_other(X) ->  X * 3.


