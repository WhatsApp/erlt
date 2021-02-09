-module(subtype_neg).

-compile([export_all, nowarn_export_all]).

-type ab() :: a | b.
-type pair_diff_elems() ::
    {a, b} | {b, a}.
-type pair_ab() ::
    {ab(), ab()}.

-spec f01(any()) -> {A, A}.
f01(X) -> X.

-spec f02(any()) -> atom().
f02(X) -> X.

-spec f03 (fun((atom()) -> any())) ->
           fun((any()) -> atom()).
f03(F) -> F.

-spec f04(a | b | c) -> (a | b).
f04(X) -> X.

-spec f05(a | b) -> none() | none().
f05(X) -> X.

-spec f06({a | b, a | b}) ->
    {a, b} | {b, a}.
f06(X) -> X.

-spec f07(pair_ab()) ->
    pair_diff_elems().
f07(X) -> X.

-spec f08({none(), none()}) -> none().
f08(X) -> X.

