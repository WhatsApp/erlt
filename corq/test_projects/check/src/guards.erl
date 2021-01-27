-module(guards).

-compile([export_all, nowarn_export_all]).

-spec test01(atom() | number()) -> atom().
test01(X) when is_atom(X) -> X.

-spec test01_if
    (atom() | number()) ->
    atom().
test01_if(X) ->
    if
        is_number(X) -> undef;
        is_atom(X) -> X
    end.

-spec test02
    (atom() | number(), atom() | number())
    -> atom().
test02(X, _) when is_atom(X) -> X;
test02(_, Y) when is_atom(Y) -> Y;
test02(_, _) -> default.

-spec test02_if
    (atom() | number(), atom() | number())
        -> atom().
test02_if(X, Y) ->
    if
        is_atom(X) -> X;
        is_atom(Y) -> Y;
        true -> default
    end.


-spec test03(atom()) -> boolean() | undef.
test03(X) when is_boolean(X) -> X;
test03(_) -> undef.

-spec test04(any()) -> number() | undef.
test04(X) when is_number(X) -> X;
test04(_) -> undef.
