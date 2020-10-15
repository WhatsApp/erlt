-file("elm_core/src/maybe.erlt", 1).


-module(maybe).


-export_type([maybe/1]).


-export([with_default/2,
         with_default/1,
         map/2,
         map/1,
         map2/3,
         map2/1,
         and_then/2,
         and_then/1]).


-export([is_just/1,destruct/3]).


-type maybe(A) ::
          {969696, maybe, maybe, just, A} |
          {969696, maybe, maybe, nothing}.


-spec with_default(A, maybe(A)) -> A.


with_default(_Default, {969696, maybe, maybe, just, Value}) ->
    Value;
with_default(Default, {969696, maybe, maybe, nothing}) ->
    Default.


-spec with_default(A) -> fun((maybe(A)) -> A).


with_default(Default) ->
    fun(Maybe) ->
           with_default(Default, Maybe)
    end.


-spec map(fun((A) -> B), maybe(A)) -> maybe(B).


map(F, {969696, maybe, maybe, just, Value}) ->
    {969696, maybe, maybe, just, F(Value)};
map(_F, {969696, maybe, maybe, nothing}) ->
    {969696, maybe, maybe, nothing}.


-spec map(fun((A) -> B)) -> fun((maybe(A)) -> maybe(B)).


map(F) ->
    fun(Maybe) ->
           map(F, Maybe)
    end.


-spec map2(fun((A, B) -> C), maybe(A), maybe(B)) -> maybe(C).


map2(F,
     {969696, maybe, maybe, just, A},
     {969696, maybe, maybe, just, B}) ->
    {969696, maybe, maybe, just, F(A, B)};
map2(_F, _Ma, _Mb) ->
    {969696, maybe, maybe, nothing}.


-spec map2(fun((A, B) -> C)) -> fun((maybe(A), maybe(B)) -> maybe(C)).


map2(F) ->
    fun(Ma, Mb) ->
           map2(F, Ma, Mb)
    end.


-spec and_then(fun((A) -> maybe(B)), maybe(A)) -> maybe(B).


and_then(Callback, {969696, maybe, maybe, just, Value}) ->
    Callback(Value);
and_then(_Callback, {969696, maybe, maybe, nothing}) ->
    {969696, maybe, maybe, nothing}.


-spec and_then(fun((A) -> maybe(B))) -> fun((maybe(A)) -> maybe(B)).


and_then(Callback) ->
    fun(Ma) ->
           and_then(Callback, Ma)
    end.


-spec is_just(maybe(_)) -> boolean().


is_just({969696, maybe, maybe, just, _}) ->
    true;
is_just({969696, maybe, maybe, nothing}) ->
    false.


-spec destruct(B, fun((A) -> B), maybe(A)) -> B.


destruct(_Default, Func, {969696, maybe, maybe, just, A}) ->
    Func(A);
destruct(Default, _Func, {969696, maybe, maybe, nothing}) ->
    Default.





