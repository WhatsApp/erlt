-module(mod03).
-compile(export_all).

-enum boxed(A) :: boxed{A}.
-enum either(A, B) :: left{A} | right {B}.
-enum rgb() :: r{} | g{} | b{}.
-enum pair(A, B) :: pair{A,B}.
-enum triple(A,B,C) :: triple{A,B,C}.
-enum list(A) :: cons{A, list(A)} | nil{}.
-enum option(A) :: none{} | some{A}.
