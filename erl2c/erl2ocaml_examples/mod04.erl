-module(mod04).
-compile(export_all).

-spec mk_unit_remote() -> mod03:unit0().
mk_unit_remote() -> mod03:unit0{}.

-spec mk_left_remote(A) -> mod03:either(A, _).
mk_left_remote(A) -> mod03:left{A}.

-spec mk_right_remote(B) -> mod03:either(_, B).
mk_right_remote(B) -> mod03:right{B}.
