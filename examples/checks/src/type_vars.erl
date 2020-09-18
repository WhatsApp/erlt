-lang([erlt]).
-module(type_vars).

-type x1(A, A) :: A.
-type x2(_) :: integer().
-type x3() :: _.

-struct y1(A, A) :: (a :: A).
-struct y2(_) :: (a :: integer()).
-struct y3 :: (a :: _).

-opaque z1(A, A) :: A.
-opaque z2(_) :: integer().
-opaque z3() :: _.

-enum e1(A, A) :: e{A}.
-enum e2(_) :: e{integer()}.
-enum e3() :: e{_}.