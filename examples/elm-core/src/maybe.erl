-lang([erl2, st]).
-module(maybe).
-compile(export_all).

-enum maybe(A) :: just{A} | nothing{}.

-spec with_default(A, maybe(A)) -> A.
with_default(_Default, just{Value}) -> Value;
with_default(Default, nothing{}) -> Default.

-spec with_default(A) -> fun((maybe(A)) -> A).
with_default(Default) ->
    fun(Maybe) -> with_default(Default, Maybe) end.

-spec map(fun((A) -> B), maybe(A)) -> maybe(B).
map(F, just{Value}) -> just{F(Value)};
map(_F, nothing{}) -> nothing{}.

-spec map(fun((A) -> B)) -> fun((maybe(A)) -> maybe(B)).
map(F) ->
    fun(Maybe) -> map(F, Maybe) end.

-spec map2(fun((A, B) -> C), maybe(A), maybe(B)) -> maybe(C).
map2(F, just{A}, just{B}) -> just{F(A, B)};
map2(_F, _Ma, _Mb) -> nothing{}.

-spec map2(fun((A, B) -> C)) -> fun((maybe(A), maybe(B)) -> maybe(C)).
map2(F) ->
    fun(Ma, Mb) -> map2(F, Ma, Mb) end.

-spec and_then(fun((A) -> maybe(B)), maybe(A)) -> maybe(B).
and_then(Callback, just{Value}) -> Callback(Value);
and_then(_Callback, nothing{}) -> nothing{}.

-spec and_then(fun((A) -> maybe(B))) -> fun((maybe(A)) -> maybe(B)).
and_then(Callback) ->
    fun(Ma) -> and_then(Callback, Ma) end.

%% FOR INTERNAL USE ONLY (Original Elm comment)

-spec is_just(maybe(_)) -> boolean().
is_just(just{_}) -> true;
is_just(nothing{}) -> false.

-spec destruct(B, fun((A) -> B), maybe(A)) -> B.
destruct(_Default, Func, just{A}) -> Func(A);
destruct(Default, _Func, nothing{}) -> Default.
