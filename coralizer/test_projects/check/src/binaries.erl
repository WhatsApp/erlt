-module(binaries).

-compile([export_all, nowarn_export_all]).

-spec test01_pos() -> binary().
test01_pos() ->
    <<>>.

-spec test02_pos(binary())
    -> {number(), binary()}.
test02_pos(<<X, Y/binary>>) -> {X, Y}.

-spec test03_pos(binary())
    -> binary().
test03_pos(<<N, _:N, Rest/binary>>) ->
    Rest.

-spec test04_neg() -> list(any()).
test04_neg() -> <<>>.

-spec test05_pos(binary())
        -> {binary(), binary()}.
test05_pos(<<H:42/binary, Rest/binary>>) ->
    {H, Rest}.

-spec test05_neg(atom()) -> binary().
test05_neg(A) -> <<A/signed>>.

-spec test06_neg(atom(), integer()) -> binary().
test06_neg(A, S) -> <<A:S>>.

-spec test07_neg(atom()) -> binary().
test07_neg(A) -> [A].

-spec test08_neg(binary()) -> float().
test08_neg(<<F/float, R/binary>>) -> F.

-spec test09_neg(integer(), integer()) -> binary().
test09_neg(I1, I2) ->
    X = <<I1, I2>>,
    X.

