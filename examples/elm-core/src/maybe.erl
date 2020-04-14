-lang([erl2, st]).
-module(maybe).

-export_type([maybe/1]).
%% TRANSFORM
-export([with_default/2, with_default/1, map/2, map/1, map2/3, map2/1, and_then/2, and_then/1]).
%% FOR INTERNAL USE ONLY (Original Elm comment)
-export([is_just/1, destruct/3]).

-enum maybe(A) :: just{A} | nothing{}.

-spec with_default(A, maybe(A)) -> A.
with_default(_Default, maybe.just{Value}) -> Value;
with_default(Default, maybe.nothing{}) -> Default.

-spec with_default(A) -> fun((maybe(A)) -> A).
with_default(Default) ->
    fun(Maybe) -> with_default(Default, Maybe) end.

-spec map(fun((A) -> B), maybe(A)) -> maybe(B).
map(F, maybe.just{Value}) -> maybe.just{F(Value)};
map(_F, maybe.nothing{}) -> maybe.nothing{}.

-spec map(fun((A) -> B)) -> fun((maybe(A)) -> maybe(B)).
map(F) ->
    fun(Maybe) -> map(F, Maybe) end.

-spec map2(fun((A, B) -> C), maybe(A), maybe(B)) -> maybe(C).
map2(F, maybe.just{A}, maybe.just{B}) -> maybe.just{F(A, B)};
map2(_F, _Ma, _Mb) -> maybe.nothing{}.

-spec map2(fun((A, B) -> C)) -> fun((maybe(A), maybe(B)) -> maybe(C)).
map2(F) ->
    fun(Ma, Mb) -> map2(F, Ma, Mb) end.

-spec and_then(fun((A) -> maybe(B)), maybe(A)) -> maybe(B).
and_then(Callback, maybe.just{Value}) -> Callback(Value);
and_then(_Callback, maybe.nothing{}) -> maybe.nothing{}.

-spec and_then(fun((A) -> maybe(B))) -> fun((maybe(A)) -> maybe(B)).
and_then(Callback) ->
    fun(Ma) -> and_then(Callback, Ma) end.

%% FOR INTERNAL USE ONLY (Original Elm comment)

-spec is_just(maybe(_)) -> boolean().
is_just(maybe.just{_}) -> true;
is_just(maybe.nothing{}) -> false.

-spec destruct(B, fun((A) -> B), maybe(A)) -> B.
destruct(_Default, Func, maybe.just{A}) -> Func(A);
destruct(Default, _Func, maybe.nothing{}) -> Default.
