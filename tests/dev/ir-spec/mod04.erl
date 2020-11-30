-file("dev/src/mod04.erlt", 1).

-module(mod04).

-export([mk_unit_remote/0,
         mk_left_remote/1,
         mk_right_remote/1,
         zero_remote/2,
         zero_remote1/2,
         un_pair_remote/1,
         funs/0,
         import1/0]).

-import(mod03, [mk_unit/0]).

-spec mk_unit_remote() -> mod03:unit0().

mk_unit_remote() -> {'$#mod03:unit0.unit0'}.

-spec mk_left_remote(A) -> mod03:either(A, _).

mk_left_remote(A) -> {'$#mod03:either.left', A}.

-spec mk_right_remote(B) -> mod03:either(_, B).

mk_right_remote(B) -> {'$#mod03:either.right', B}.

-spec zero_remote(mod03:unit0(), V) -> V.

zero_remote(U, Val) ->
    case U of {'$#mod03:unit0.unit0'} -> Val end.

-spec zero_remote1(mod03:unit0(), V) -> V.

zero_remote1({'$#mod03:unit0.unit0'}, Val) -> Val.

-spec un_pair_remote(mod03:pair(A, B)) -> {A, B}.

un_pair_remote(Pair) ->
    case Pair of {'$#mod03:pair.pair', A, B} -> {A, B} end.

-spec funs() -> [fun(() -> mod03:unit0())].

funs() ->
    [fun mod03:mk_unit/0,
     fun mod04:mk_unit_remote/0,
     fun mk_unit_remote/0,
     fun () -> {'$#mod03:unit0.unit0'} end].

-spec import1() -> mod03:unit0().

import1() -> mod03:mk_unit().



