-module(comprehensions_todo).

-compile([export_all, nowarn_export_all]).

-spec test01
    ([atom() | number()]) ->
    [atom()].
test01(L) ->
    [X || X <- L, is_atom(X)].

-spec test02
    ([atom() | boolean()]) ->
    [boolean()].
test02(L) ->
    [X || X <- L, X].
