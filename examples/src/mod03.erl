-lang([erl2, st]).
-module(mod03).
-compile(export_all).

-enum unit0() :: unit0{}.
-enum boxed(A) :: boxed{A}.
-enum either(A, B) :: left{A} | right {B}.
-enum rgb() :: r{} | g{} | b{}.
-enum pair(A, B) :: pair{A,B}.
-enum triple(A,B,C) :: triple{A,B,C}.
-enum list(A) :: cons{A, list(A)} | nil{}.
-enum option(A) :: none{} | some{A}.

-spec mk_unit() -> unit0().
mk_unit() -> unit0{}.

-spec mk_box(A) -> boxed(A).
mk_box(A) -> boxed{A}.

-spec mk_left(A) -> either(A, _).
mk_left(A) -> left{A}.

-spec mk_right(B) -> either(_, B).
mk_right(B) -> right{B}.

-spec zero(unit0(), V) -> V.
zero(unit0{}, Val) -> Val.

-spec unbox(boxed(E)) -> E.
unbox(Boxed) ->
  case Boxed of
    boxed{Elem} -> Elem
  end.

-spec un_either(either(A,A)) -> A.
un_either(Either) ->
  case Either of
    left{Elem} -> Elem;
    right{Elem} -> Elem
  end.

-spec un_pair(pair(A,B)) -> {A,B}.
un_pair(Pair) ->
  case Pair of
    pair{A, B} -> {A, B}
  end.

-spec first(pair(A,_)) -> A.
first(pair{F,_}) -> F.

-spec second(pair(_,B)) -> B.
second(pair{_,S}) -> S.
