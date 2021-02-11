-module(subtype_pos).

-compile([export_all, nowarn_export_all]).

-type ab() :: a | b.
-type pair_diff_elems() ::
    {a, b} | {b, a}.
-type pair_ab() ::
    {ab(), ab()}.

-spec f01({A, A}) -> any().
f01(X) -> X.

-spec f02(atom()) -> any().
f02(X) -> X.

-spec f03 (fun((any()) -> atom())) ->
    fun((atom()) -> any()).
f03(F) -> F.

-spec f04(a | b) -> (a | b | c).
f04(X) -> X.

-spec f05(none() | none()) -> a | b.
f05(X) -> X.

-spec f06({a, b} | {b, a}) ->
           {a | b, a | b}.
f06(X) -> X.

-spec f07(pair_diff_elems()) ->
    pair_ab().
f07(X) -> X.

-spec f08(none()) -> {none(), none()}.
f08(X) -> X.

-spec map01(#{}) -> map().
map01(M) -> M.

-spec map02(#{a := atom()})
    -> #{a => atom()}.
map02(M) -> M.

-spec map03(#{a := atom(), b => atom()})
        -> #{atom() => atom()}.
map03(M) -> M.

-spec map04(#{
    a := a | b,
    b => a | b
}) -> #{a | b => a | b}.
map04(M) -> M.

-spec map05(#{
    a := a | b,
    b => a | b
}) -> #{a | b => a | b | c}.
map05(M) -> M.

-spec map06(#{atom() => integer()})
    -> #{any() => integer()}.
map06(M) -> M.

-spec map07(#{atom() => integer()})
        -> #{atom() => any()}.
map07(M) -> M.

-spec map08(#{})
        -> #{atom() => any()}.
map08(M) -> M.
