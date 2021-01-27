-module(misc).

%% The majority of these tests are just
%% to get some code coverage.
%% Many of them do not have any special meaning.

-compile([export_all, nowarn_export_all]).

-import(misc_lib, [boolean_id/1]).

-spec test01(any()) -> number().
test01(-4 = X) -> X.

-spec test02(any()) -> number().
test02(6 / 3 = Y) -> Y.

-spec test03_neg(any()) -> atom().
test03_neg(-4 = X) -> X.

-spec test04_neg(any()) -> atom().
test04_neg(6 / 3 = Y) -> Y.

-spec test05_neg(any()) -> any().
test05_neg(X) -> + X.

-spec test06_neg(any()) -> atom().
test06_neg(X) when is_number(X) -> + X.

-spec test07(boolean()) -> atom().
test07(X) ->
    not X.

-spec test08_neg(boolean()) -> number().
test08_neg(X) ->
    not X.

-spec test09(any(), any()) -> number().
test09(X, Y) when
    is_number(X),
    is_number(Y) -> X + Y.

-spec test10_neg (any(), any())
               -> number().
test10_neg(X, Y) when
    is_number(X) -> X + Y.

-spec test11_neg (number(), number())
        -> atom().
test11_neg(X, Y) -> X + Y.

-spec test12_neg (atom())
        -> number().
test12_neg(X) -> -X.

-spec test13_neg (any(), any())
        -> boolean().
test13_neg(X, Y) -> X or Y.

-spec test14 (boolean(), atom())
        -> atom().
test14(X, Y) -> X orelse Y.

-spec test15 (boolean(), atom())
        -> atom().
test15(X, Y) -> X andalso Y.

-spec test16_neg (boolean(), boolean())
        -> number().
test16_neg(X, Y) -> X or Y.

-spec test17_neg (boolean(), atom())
        -> boolean().
test17_neg(X, Y) -> X or Y.

-spec test18_neg (boolean(), atom())
        -> number().
test18_neg(X, Y) -> X orelse Y.

-spec test19_neg (boolean(), number())
        -> {number()}.
test19_neg(X, Y) -> X andalso Y.

-spec test20_neg(number()) -> [atom()].
test20_neg(X) -> [- X].

-spec test21_neg (number(), number())
              -> [atom()].
test21_neg(X, Y) -> [X / Y].

-spec test22_neg (boolean())
        -> [number()].
test22_neg(X) -> [not X].

-spec test23_neg (boolean(), boolean())
        -> [number()].
test23_neg(X, Y) -> [X or Y].

-spec test24_neg (boolean(), atom())
        -> [number()].
test24_neg(X, Y) -> [X orelse Y].

-spec test25_pos
  ([boolean()]) -> boolean() | undefined.
test25_pos([]) -> undefined;
test25_pos([X]) -> X;
test25_pos([_|T]) -> test25_pos(T).

-spec test26_pos(X) -> X.
test26_pos(X) -> X.

-spec test27_pos(X) -> [X].
test27_pos(X) -> [X].

-spec test28_neg(atom()) -> boolean().
test28_neg(B) -> boolean_id(B).

-spec test29_neg() -> [].
test29_neg() -> [true, false].

-spec test30_neg() -> atom.
test30_neg() -> 1.

-spec test31_neg() -> atom.
test31_neg() -> fun test30_neg/0.

-spec test32_neg() -> atom.
test32_neg() -> fun misc_lib:boolean_id/1.

-spec test33_pos(a | b, b | c) -> b.
test33_pos(AB, BC) ->
    case AB of BC -> BC end.

-spec test34_pos({a | b}, {b | c}) -> {b}.
test34_pos(AB, BC) ->
    case AB of BC -> BC end.

-spec test35_pos([a | b], [b | c]) -> [b].
test35_pos(AB, BC) ->
    case AB of BC -> BC end.

-spec test36_pos(F1, F2) -> F3
    when F1 :: fun((a) -> a | z),
         F2 :: fun((b) -> b | z),
         F3 :: fun((a | b) -> z).
test36_pos(F1, F2) ->
    case F1 of F2 -> F2 end.

-spec test37_pos (any(), any())
              -> {number(), number()}.
test37_pos(X, Y) when X + Y > 0 -> {X, Y}.

-spec test38_pos (any())
        -> {number()}.
test38_pos(X) when bnot X > 0 -> {X}.

-spec test39_pos
    ({atom(), atom()} | number())
  -> {atom(), atom()}.
test39_pos({X, Y}) -> {X, Y};
test39_pos(_) -> {a, b}.

-spec test40_pos
    (number()) -> {none(), none()}.
test40_pos({X, Y}) -> {X, Y}.

-spec test41_pos
    (number()) -> {none(), [none()]}.
test41_pos([X | Y]) -> {X, Y}.

-spec test42_pos
    (any()) -> {any(), [any()]}.
test42_pos([X | Y]) -> {X, Y}.

-spec test43_pos
    (any()) -> none().
test43_pos([_ | Y]) when Y + 1 > 0 -> Y.

-spec test44_neg(any()) -> any().
test44_neg(X) ->
    case X of
        Z -> ok
    end,
    Z.

-spec test45_neg(any()) -> atom().
test45_neg(X) ->
    case X of
        _ -> Z = ok
    end,
    Z.

-spec test46_neg() -> number().
test46_neg() -> false.

-spec test47_neg() -> number().
test47_neg() -> [].

-spec test48_neg() -> atom().
test48_neg() -> test47_neg().

-spec test49_neg() -> atom().
test49_neg() -> misc:test47_neg().

-spec test50_pos() -> number().
test50_pos() -> misc:test47_neg().

-spec test51_pos() -> number().
test51_pos() -> test47_neg().

-spec test52_pos() ->
      fun(() -> number()).
test52_pos() ->
    fun test47_neg/0.

-spec test53_pos() ->
    fun(() -> number()).
test53_pos() ->
    fun misc:test47_neg/0.

test54_unspecced() -> ok.

-spec test55_neg() ->
    fun(() -> number()).
test55_neg() ->
    fun misc:test54_unspecced/0.

-spec test56_neg() ->
    fun(() -> number()).
test56_neg() ->
    fun test54_unspecced/0.

-spec test57_pos() -> {atom(), number()}.
test57_pos() ->
    A = atom,
    N = 1,
    {A, N}.

-spec test58_pos
    (boolean(), atom(), number()) ->
    atom() | number().
test58_pos(B, A, N) ->
    Res =
    begin
    if B -> A; true -> N end
    end,
    Res.

-spec test59_pos() -> number().
test59_pos() ->
    X = misc:test47_neg(),
    X.

-spec test60_pos() -> number().
test60_pos() ->
    X = test47_neg(),
    X.

-spec test61_neg() -> number().
test61_neg() ->
    X = misc:test54_unspecced(),
    X.

-spec test62_neg() -> number().
test62_neg() ->
    X = test54_unspecced(),
    X.

-spec test63_pos() ->
    fun(() -> number()).
test63_pos() ->
    Fun = fun misc:test47_neg/0,
    Fun.

-spec test64_pos() ->
    fun(() -> number()).
test64_pos() ->
    Fun = fun test47_neg/0,
    Fun.

-spec test65_pos() -> [any()].
test65_pos() ->
    L = [],
    L.

-spec test66_pos() ->
    {atom(), number()}.
test66_pos() ->
    Result =
        begin
            X = a,
            Y = 1,
            {X, Y}
        end,
    Result.

-spec test67_pos
    (atom(), number()) ->
    [atom() | number()].
test67_pos(A, N) ->
    Result =
        begin
            Head = A,
            Tail = [N],
            [Head | Tail]
        end,
    Result.

-spec test68_pos({atom()}) -> none().
test68_pos({E, _}) -> E.

-spec test69_pos
    ([atom()] | [number()]) ->
    [atom() | number()].
test69_pos([H | T]) -> [H | T].

-spec test70_neg() ->
    [atom() | number()].
test70_neg() ->
    catch test69_pos([atom]).

-spec test71_neg() ->
    [atom() | number()].
test71_neg() ->
    catch test69_pos(atom).

-spec test72_neg() ->
    any().
test72_neg() ->
    catch test69_pos(atom).

-spec test73_pos() ->
    any().
test73_pos() ->
    catch test69_pos([atom]).

-spec test74_pos() ->
    any().
test74_pos() ->
    X = (catch test69_pos([atom])),
    X.

-spec test75_pos() -> ok.
test75_pos() ->
    try
        ok
    of
        ok -> ok
    after
        nook
    end.

-spec test76_pos(any()) -> atom().
test76_pos(A) when is_atom(A) -> A;
test76_pos(_) ->
    erlang:throw({error, not_an_atom}).

-spec test77_neg(any()) -> atom().
test77_neg(A) -> catch(test76_pos(A)).

-spec test78_pos() -> any().
test78_pos() ->
    receive
        X -> X
    end.

-spec test79_neg() -> atom().
test79_neg() ->
    receive
        X -> X
    end.

-spec test80_neg(any()) -> atom().
test80_neg(Timeout) ->
    receive
        X -> X
    after Timeout ->
        default
    end.

-spec test81_neg(any()) -> atom().
test81_neg(Timeout) ->
    receive
        X when is_atom(X) -> X
    after Timeout ->
        default
    end.

-spec test82_pos() -> atom().
test82_pos() ->
    A = atom,
    Msg = receive
        A -> A
    after 10 ->
        default
    end,
    {Msg}.

-spec test83_pos(integer()) -> atom().
test83_pos(Timeout) ->
    receive
        _ -> atom
    after Timeout ->
        default
    end.
