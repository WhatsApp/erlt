-lang([erl2, st]).
-module(result).
-depends_on([maybe]).

-export_type([result/2]).
-export([with_default/2, with_default/1]).
-export([map/2, map/1, map2/3, map2/1]).
-export([and_then/2, and_then/1, map_error/2, map_error/1]).
-export([to_maybe/1, from_maybe/2, from_maybe/1]).
-export([is_ok/1]).

-enum result(Error, Value) :: ok{Value} | err{Error}.

-spec with_default(A, result(_, A)) -> A.
with_default(_Def, ok{A}) -> A;
with_default(Def, err{_E}) -> Def.

-spec with_default(A) -> fun((result(_, A)) -> A).
with_default(Def) ->
    fun(Res) -> with_default(Def, Res) end.

-spec map(fun((A) -> B), result(X, A)) -> result(X, B).
map(Func, ok{A}) -> ok{Func(A)};
map(_Func, err{E}) -> err{E}.

-spec map(fun((A) -> B)) -> fun((result(X, A)) -> result(X, B)).
map(Func) ->
    fun(Res) -> map(Func, Res) end.

-spec map2(fun((A, B) -> C), result(X, A), result(X, B)) -> result(X, C).
map2(_Func, err{X}, _) -> err{X};
map2(_Func, ok{_}, err{X}) -> err{X};
map2(Func, ok{A}, ok{B}) -> ok{Func(A, B)}.

-spec map2(fun((A, B) -> C)) -> fun((result(X, A), result(X, B)) -> result(X, C)).
map2(Func) ->
    fun(ResA, ResB) -> map2(Func, ResA, ResB) end.

-spec and_then(fun((A) -> result(X,B)), result(X,A)) -> result(X,B).
and_then(Callback, ok{Value}) -> Callback(Value);
and_then(_Callback, err{Msg}) -> err{Msg}.

-spec and_then(fun((A) -> result(X,B))) -> fun((result(X,A)) -> result(X,B)).
and_then(Callback) ->
    fun(Res) -> and_then(Callback, Res) end.

-spec map_error(fun((X) -> Y), result(X, A)) -> result(Y, A).
map_error(_F, ok{V}) -> ok{V};
map_error(F, err{E}) -> err{F(E)}.

-spec map_error(fun((X) -> Y)) -> fun((result(X, A)) -> result(Y, A)).
map_error(F) ->
    fun(Res) -> map_error(F, Res) end.

-spec to_maybe(result(_, A)) -> maybe:maybe(A).
to_maybe(ok{V}) -> maybe.just{V};
to_maybe(err{_}) -> maybe.nothing{}.

-spec from_maybe(X, maybe:maybe(A)) -> result(X, A).
from_maybe(_Err, maybe.just{V}) -> ok{V};
from_maybe(Err, maybe.nothing{}) -> err{Err}.

-spec from_maybe(X) -> fun((maybe:maybe(A)) -> result(X, A)).
from_maybe(Err) ->
    fun(Maybe) -> from_maybe(Err, Maybe) end.

%% FOR INTERNAL USE ONLY (Original Elm comment)

-spec is_ok(result(_, _)) -> boolean().
is_ok(ok{_}) -> true;
is_ok(err{_}) -> false.
