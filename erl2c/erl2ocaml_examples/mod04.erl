-module(mod04).
-compile(export_all).

-spec mk_unit_remote() -> mod03:unit0().
mk_unit_remote() -> mod03:unit0{}.

-spec mk_left_remote(A) -> mod03:either(A, _).
mk_left_remote(A) -> mod03:left{A}.

-spec mk_right_remote(B) -> mod03:either(_, B).
mk_right_remote(B) -> mod03:right{B}.

-spec zero_remote(mod03:unit0(), V) -> V.
zero_remote(U, Val) ->
  case U of
    mod03:unit0{} -> Val
  end.

%% TODO - enable when https://github.com/WhatsApp/erl2/issues/3 is fixed
%%-spec zero_remote1(mod03:unit0(), V) -> V.
%%zero_remote1(mod03:unit0{}, Val) -> Val.

-spec un_pair_remote(mod03:pair(A,B)) -> {A,B}.
un_pair_remote(Pair) ->
  case Pair of
    mod03:pair{A, B} -> {A, B}
end.
