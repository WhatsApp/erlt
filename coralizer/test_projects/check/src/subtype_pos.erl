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
