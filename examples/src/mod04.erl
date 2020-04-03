-lang([erl2, st]).
-module(mod04).
-compile(export_all).

-depends_on([mod03]).

-spec mk_unit_remote() -> mod03:unit0().
mk_unit_remote() -> mod03:unit0{}.

-spec mk_unit_remote_dot() -> mod03:unit0().
mk_unit_remote_dot() -> mod03.unit0{}.

-spec mk_left_remote(A) -> mod03:either(A, _).
mk_left_remote(A) -> mod03:left{A}.

-spec mk_left_remote_dot(A) -> mod03:either(A, _).
mk_left_remote_dot(A) -> mod03.left{A}.

-spec mk_right_remote(B) -> mod03:either(_, B).
mk_right_remote(B) -> mod03:right{B}.

-spec mk_right_remote_dot(B) -> mod03:either(_, B).
mk_right_remote_dot(B) -> mod03.right{B}.

-spec zero_remote(mod03:unit0(), V) -> V.
zero_remote(U, Val) ->
  case U of
    mod03:unit0{} -> Val
  end.

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

-spec un_pair_remote(mod03:pair(A,B)) -> {A,B}.
un_pair_remote(Pair) ->
  case Pair of
    mod03:pair{A, B} -> {A, B}
end.

-spec un_pair_remote_dot(mod03:pair(A,B)) -> {A,B}.
un_pair_remote_dot(Pair) ->
  case Pair of
    mod03.pair{A, B} -> {A, B}
end.
