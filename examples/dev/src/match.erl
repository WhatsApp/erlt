-lang([erl2, st]).
-module(match).

%% Testing erl2ocaml for cases when `Pat = Exp` (match expr)
%% should be interpreted as an expression.

-export([f1/0, f2/0, f4/0, f5/2]).

id(X) -> X.

-spec f1() -> {integer(), integer()}.
f1() ->
    {_X, _Y} = {1, 2}.

-spec f2() -> integer().
f2() ->
    Y = 1,
    _X = Y.

f3(X, T) ->
    case X of
        1 -> _Y = 2;
        2 -> _Y = 3;
        _ -> Z = T, _Y = Z
    end.

-spec f4() -> integer().
f4() ->
    f3(_Y = 1, 4),
    f3(2 = 1, 3 = 4),
    id([1, 2] = [3, 4]),
    0.

-spec f5({A, B}, {A, B}) -> boolean().
f5(P1, P2) ->
    ({_X1, _Y1} = P1) == ({_X2, _Y2} = P2).
