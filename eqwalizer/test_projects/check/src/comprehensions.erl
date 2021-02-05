-module(comprehensions).

-compile([export_all, nowarn_export_all]).

-spec gen_atom(any()) -> atom().
gen_atom(A) when is_atom(A) -> A;
gen_atom(_) -> not_atom.

-spec gen_number(any()) -> number().
gen_number(N) when is_number(N) -> N;
gen_number(_) -> 0.

-spec test01
    ([{atom()} | {atom(), number()}]) ->
    [atom()].
test01(L) ->
    [A || {A} <- L].

-spec test02_neg
    ([{atom()} | {atom(), number()}]) ->
    [atom()].
test02_neg(L) ->
    [N || {_, N} <- L].

-spec test03
    ([{atom()} | {atom(), number()}]) ->
    {[atom()], x}.
test03(L) ->
    Atoms = [gen_atom(X) || X <- L],
    X = x,
    {Atoms, X}.

-spec test04_neg(any()) -> [any()].
test04_neg(L) ->
    [X || X <- L].

-spec test05(binary()) -> [number()].
test05(B) ->
    [Y || <<Y>> <= B ].

-spec test06_neg(binary()) -> list(any()).
test06_neg(B) ->
    << Y || <<Y>> <= B >>.

-spec test07(list(binary())) -> binary().
test07(LB) ->
    << Y || Y <- LB >>.

-spec test08_neg(binary()) -> binary().
test08_neg(B) ->
    << Y || Y <- B >>.

-spec test09_neg(list(binary())) -> binary().
test09_neg(LB) ->
    << Y || <<Y>> <= LB >>.

-spec test10(binary()) -> [number()].
test10(LB) ->
    [ Y || <<Y>> <= LB ].

-spec test11_neg(binary()) -> [binary()].
test11_neg(LB) ->
    [ Y || <<Y>> <= LB ].

-spec test12(binary()) -> [binary()].
test12(LB) ->
    << Y || <<Y>> <= LB >>.

-spec test13_neg(binary()) -> binary().
test13_neg(LB) ->
    << Y || <<Y>> <= LB >>.

-spec test14(binary()) -> binary().
test14(LB) ->
    << <<Y>> || <<Y>> <= LB >>.

-spec test15_neg(binary()) -> binary().
test15_neg(LB) ->
    << <<Y>> ||
        <<Y>> <= LB,
        gen_atom(Y) >>.

-spec test16_neg(any()) -> [any()].
test16_neg(L) ->
    Res = [X || X <- L],
    Res.

-spec test17(binary()) -> [number()].
test17(LB) ->
    Res = [ Y || <<Y>> <= LB ],
    Res.

-spec test18_neg(boolean()) -> [boolean()].
test18_neg(L) ->
    Res = [X || X <- L, X],
    Res.

-spec test19([boolean()]) -> [binary()].
test19(L) ->
    Res = [erlang:atom_to_binary(X) ||
            X <- L, X],
    Res.

-spec test20(binary()) -> binary().
test20(LB) ->
    Res = << <<Y>> || <<Y>> <= LB >>,
    Res.

-spec test21_neg(list(binary())) -> binary().
test21_neg(LB) ->
    Res = << Y || <<Y>> <= LB >>,
    Res.

-spec test22_neg(binary()) -> binary().
test22_neg(B) ->
    Res = << Y || Y <- B >>,
    Res.

-spec test23([binary()]) -> binary().
test23(B) ->
    Res = << Y || Y <- B >>,
    Res.

-spec test24([boolean()]) -> binary().
test24(B) ->
    Res = << (erlang:atom_to_binary(Y)) ||
            Y <- B, Y >>,
    Res.

-spec num_atom(number(), atom())
    -> number().
num_atom(_, _) -> 3.

-spec test25
    ([{number()} | [atom()]]) ->
    [number()].
test25(L) ->
    [num_atom(A, B) + 1 ||
        {A} <- L, [B] <- L].
