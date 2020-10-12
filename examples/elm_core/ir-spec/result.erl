-file("elm_core/src/result.erlt", 1).

-module(result).

-export_type([result/2]).

-export([with_default/2, with_default/1]).

-export([map/2, map/1, map2/3, map2/1]).

-export([and_then/2,
         and_then/1,
         map_error/2,
         map_error/1]).

-export([to_maybe/1, from_maybe/2, from_maybe/1]).

-export([is_ok/1]).

-import_type({maybe, [{maybe, 1}]}).

-type result(Error, Value) :: {969696,
                               result,
                               result,
                               ok,
                               Value} |
                              {969696, result, result, err, Error}.

-spec with_default(A, result(_, A)) -> A.

with_default(_Def, {969696, result, result, ok, A}) ->
    A;
with_default(Def, {969696, result, result, err, _E}) ->
    Def.

-spec with_default(A) -> fun((result(_, A)) -> A).

with_default(Def) ->
    fun (Res) -> with_default(Def, Res) end.

-spec map(fun((A) -> B), result(X, A)) -> result(X, B).

map(Func, {969696, result, result, ok, A}) ->
    {969696, result, result, ok, Func(A)};
map(_Func, {969696, result, result, err, E}) ->
    {969696, result, result, err, E}.

-spec map(fun((A) -> B)) -> fun((result(X,
                                        A)) -> result(X, B)).

map(Func) -> fun (Res) -> map(Func, Res) end.

-spec map2(fun((A, B) -> C), result(X, A),
           result(X, B)) -> result(X, C).

map2(_Func, {969696, result, result, err, X}, _) ->
    {969696, result, result, err, X};
map2(_Func, {969696, result, result, ok, _},
     {969696, result, result, err, X}) ->
    {969696, result, result, err, X};
map2(Func, {969696, result, result, ok, A},
     {969696, result, result, ok, B}) ->
    {969696, result, result, ok, Func(A, B)}.

-spec map2(fun((A, B) -> C)) -> fun((result(X, A),
                                     result(X, B)) -> result(X, C)).

map2(Func) ->
    fun (ResA, ResB) -> map2(Func, ResA, ResB) end.

-spec and_then(fun((A) -> result(X, B)),
               result(X, A)) -> result(X, B).

and_then(Callback,
         {969696, result, result, ok, Value}) ->
    Callback(Value);
and_then(_Callback,
         {969696, result, result, err, Msg}) ->
    {969696, result, result, err, Msg}.

-spec and_then(fun((A) -> result(X,
                                 B))) -> fun((result(X, A)) -> result(X, B)).

and_then(Callback) ->
    fun (Res) -> and_then(Callback, Res) end.

-spec map_error(fun((X) -> Y),
                result(X, A)) -> result(Y, A).

map_error(_F, {969696, result, result, ok, V}) ->
    {969696, result, result, ok, V};
map_error(F, {969696, result, result, err, E}) ->
    {969696, result, result, err, F(E)}.

-spec map_error(fun((X) -> Y)) -> fun((result(X,
                                              A)) -> result(Y, A)).

map_error(F) -> fun (Res) -> map_error(F, Res) end.

-spec to_maybe(result(_, A)) -> maybe:maybe(A).

to_maybe({969696, result, result, ok, V}) ->
    {969696, maybe, maybe, just, V};
to_maybe({969696, result, result, err, _}) ->
    {969696, maybe, maybe, nothing}.

-spec from_maybe(X, maybe:maybe(A)) -> result(X, A).

from_maybe(_Err, {969696, maybe, maybe, just, V}) ->
    {969696, result, result, ok, V};
from_maybe(Err, {969696, maybe, maybe, nothing}) ->
    {969696, result, result, err, Err}.

-spec from_maybe(X) -> fun((maybe:maybe(A)) -> result(X,
                                                      A)).

from_maybe(Err) ->
    fun (Maybe) -> from_maybe(Err, Maybe) end.

-spec is_ok(result(_, _)) -> boolean().

is_ok({969696, result, result, ok, _}) -> true;
is_ok({969696, result, result, err, _}) -> false.



