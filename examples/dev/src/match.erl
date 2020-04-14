-lang([erl2, st]).
-module(match).

-export([f1/0, f2/0, f4/0, f5/2, f6/2, f7/2, f8/2, f9/2, f10/0, f11/1, f12/1, f13/2, f14/1]).

id(X) -> X.

%% Testing erl2ocaml for cases when `Pat = Exp` (match expr)
%% should be interpreted as an expression.

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


%% Testing erl2ocaml for patterns in clauses with repeated variables.

-spec f6(A, A) -> {A, A}.
f6(X, X) ->
    {X, X}.

-spec f7(A, A) -> {A, A}.
f7(X, Y = X) ->
    {X, Y}.

-spec f8({A, B}, {A, B}) -> ({A, B}).
f8({X, Y} = Z, {X, Y} = Z) ->
    Z.

-spec f9({A, B}, {A, B}) -> ({A, B}).
f9(P1, P2) ->
    case {P1, P2} of
        {{X, Y} = Z, {X, Y} = Z} -> Z
    end.

%% Testing erl2ocaml for patterns bodies with repeated vars.
-spec f10() -> {integer(), integer()}.
f10() ->
    {X, X} = {1, 1}.

-spec f11([{E, E}]) -> E.
f11(L) ->
    [{E,E}, {E, E}] = L,
    E.

%% Testing of complex match patterns
%% In clauses

-spec f12({{A, A}, {A, A}}) -> {A, A}.
f12({{E1, E2}, {E2, E1}} = {E6, E6}) ->
    E6.


-spec f13({A, A}, A) -> A.
f13(_P = {E3, E3}, E3) ->
    _X = E3.


%% Testing of complex match patterns
%% In bodies

-spec f14({{A, A}, {A, A}}) -> {A, A}.
f14(P) ->
    ({{El1, El2}, {El1, El2}} = {X, X}) = P,
    X.

