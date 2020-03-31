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
