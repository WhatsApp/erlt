-lang([erl2, st]).

-module(etf18).

-export([guard1/2, guard2/3, guard3/2, guard4/1, guard5/1]).

-spec guard1(integer(), integer()) -> boolean().
guard1(X, Y) when X == 1, Y == 1; X =/= Y ->
    true;
guard1(X, Y) when X > Y ->
    false;
guard1(_X, _Y) ->
    false.

-spec guard2([A], A, [A]) -> [A].
guard2(X, Y, []) ->
    case X of
        [X1 | Xs] when X1 == Y -> Xs;
        Xs -> Xs
    end;
guard2(X, _, Z) when X == Z ->
    Z;
guard2(_, _, Z) ->
    Z.

guard3(X, Y) when X.id == Y -> true.

guard4(X) when X#{id := 1} == #{id => 1} -> true.

guard5(T) when erlang:length(T) > 0; T == []; T == [2 | []] -> true.
