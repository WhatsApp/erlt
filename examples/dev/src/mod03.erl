-lang([erl2, st]).
-module(mod03).
-export_type([unit0/0, boxed/1, either/2, rgb/0, pair/2, triple/3, my_list/1, option/1]).
-export([
  mk_rgb/0, mk_triple/1, mk_none/0, mk_unit/0, mk_box/1, mk_left/1, mk_right/1,
  zero/2, unbox/1, un_either/1, un_pair/1,
  first/1, second/1
]).

-enum unit0() :: unit0{}.
-enum boxed(A) :: boxed{A}.
-enum either(A, B) :: left{A} | right {B}.
-enum rgb() :: r{} | g{} | b{}.
-enum pair(A, B) :: pair{A,B}.
-enum triple(A,B,C) :: triple{A,B,C}.
-enum my_list(A) :: cons{A, my_list(A)} | nil{}.
-enum option(A) :: none{} | some{A}.

-spec mk_rgb() -> rgb().
mk_rgb() -> rgb.r{}.

-spec mk_triple(A) -> triple(A, A, A).
mk_triple(A) -> triple.triple{A, A, A}.

-spec mk_none() -> option(_).
mk_none() -> option.none{}.

-spec mk_unit() -> unit0().
mk_unit() -> unit0.unit0{}.

-spec mk_box(A) -> boxed(A).
mk_box(A) -> boxed.boxed{A}.

-spec mk_left(A) -> either(A, _).
mk_left(A) -> either.left{A}.

-spec mk_right(B) -> either(_, B).
mk_right(B) -> either.right{B}.

-spec zero(unit0(), V) -> V.
zero(unit0.unit0{}, Val) -> Val.

-spec unbox(boxed(E)) -> E.
unbox(Boxed) ->
  case Boxed of
    boxed.boxed{Elem} -> Elem
  end.

-spec un_either(either(A,A)) -> A.
un_either(Either) ->
  case Either of
    either.left{Elem} -> Elem;
    either.right{Elem} -> Elem
  end.

-spec un_pair(pair(A,B)) -> {A,B}.
un_pair(Pair) ->
  case Pair of
    pair.pair{A, B} -> {A, B}
  end.

-spec first(pair(A,_)) -> A.
first(pair.pair{F,_}) -> F.

-spec second(pair(_,B)) -> B.
second(pair.pair{_,S}) -> S.
