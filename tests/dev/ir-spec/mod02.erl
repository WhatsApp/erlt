-file("dev/src/mod02.erlt", 1).

-module(mod02).

-eqwalizer_unchecked([]).

-export([odd/1,
         id/1,
         id_caller/1,
         id_rec/1,
         even/1,
         mod01call/1,
         mk_unit/0]).

-spec odd(integer()) -> boolean().

odd(X) -> even(X - 1).

-spec id(X) -> X.

id(X) -> X.

-spec id_caller(X) -> X.

id_caller(X) -> id(X).

-spec id_rec(X) -> X.

id_rec(X) -> id_rec(X).

-spec even(integer()) -> boolean().

even(0) -> true;
even(X) -> odd(X - 1).

-spec mod01call(X) -> X.

mod01call(X) -> mod01:mod01F(X).

-spec mk_unit() -> integer().

mk_unit() -> 1.



