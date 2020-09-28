-lang(st).

-module(etf06).

-export([same/2, same/3, same1/3, foo/1, same2/1]).

same(X, Y = X) -> Y;
same(X, _) -> X.

same(X, Y, X = Y = Z) -> Z;
same(_, Y, _) -> Y.

same1(X, Y, (X = Y) = Z) -> Z;
same1(_, Y, _) -> Y.

same2({X, X} = Z) -> Z;
same2(Y) -> Y.

foo(Y) ->
    X = Y,
    3 = X.
