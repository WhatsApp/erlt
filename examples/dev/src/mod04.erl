-lang([erl2, st]).
-module(mod04).

-depends_on([mod03]).

-export([
  mk_unit_remote_dot/0, mk_left_remote_dot/1, mk_right_remote_dot/1,
  zero_remote_dot/2, zero_remote1_dot/2,
  un_pair_remote_dot/1
]).

-spec mk_unit_remote_dot() -> mod03:unit0().
mk_unit_remote_dot() -> mod03.unit0{}.

-spec mk_left_remote_dot(A) -> mod03:either(A, _).
mk_left_remote_dot(A) -> mod03.left{A}.

-spec mk_right_remote_dot(B) -> mod03:either(_, B).
mk_right_remote_dot(B) -> mod03.right{B}.

-spec zero_remote_dot(mod03:unit0(), V) -> V.
zero_remote_dot(U, Val) ->
  case U of
    mod03.unit0{} -> Val
end.

%% This is not supported - see https://github.com/WhatsApp/erl2/issues/3
%%-spec zero_remote1(mod03:unit0(), V) -> V.
%%zero_remote1(mod03:unit0{}, Val) -> Val.

-spec zero_remote1_dot(mod03:unit0(), V) -> V.
zero_remote1_dot(mod03.unit0{}, Val) -> Val.

-spec un_pair_remote_dot(mod03:pair(A,B)) -> {A,B}.
un_pair_remote_dot(Pair) ->
  case Pair of
    mod03.pair{A, B} -> {A, B}
end.
