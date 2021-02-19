-module(elab_clause).

-compile([export_all, nowarn_export_all]).

-spec foo(any()) -> {atom(), atom()}.
foo(_) -> {any, any}.

-spec bar(any()) ->
    {atom(), atom()} | {number()}.
bar(true) -> {any, any};
bar(_) -> {0}.

-spec app_foo(any()) -> atom().
app_foo(X) ->
    Res = case foo(X) of
              {Y, _} -> Y
          end,
    Res.

-spec app_foo_neg(any()) -> binary().
app_foo_neg(X) ->
    Res = case foo(X) of
              {_, Y} -> Y
          end,
    Res.

-spec app_bar(any()) ->
    {atom()} | number().
app_bar(X) ->
    Res = case bar(X) of
              {Y, _} -> {Y};
              {N} -> N
          end,
    Res.

-spec app_bar_neg(any()) ->
    {atom()} | number().
app_bar_neg(X) ->
    Res = case bar(X) of
              {Y, _} -> Y;
              {N} -> {N}
          end,
    Res.

-spec catch_foo(any()) -> atom().
catch_foo(X) ->
    Res =
        try foo(X)
        of {Y, _} -> Y
        catch
            A:_  -> A
        end,
    Res.

-spec catch_foo1(any()) -> atom().
catch_foo1(X) ->
    try foo(X)
    of {Y, _} -> Y
    catch
        A:_  -> A
    end.

-spec catch_foo1_neg(any()) -> number().
catch_foo1_neg(X) ->
    Res =
        try foo(X)
        of {Y, _} -> 1
        catch
            A:_  -> A
        end,
    Res.

-spec catch_foo2_neg(any()) -> number().
catch_foo2_neg(X) ->
    Res =
        try foo(X)
        of {Y, _} -> 1
        catch
            _:_:Stack  -> Stack
        end,
    Res.

-spec catch_foo3_neg(any()) -> number().
catch_foo3_neg(X) ->
    try foo(X)
    of {Y, _} -> 1
    catch
        A:_  -> A
    end.

-spec catch_foo4_neg(any()) -> number().
catch_foo4_neg(X) ->
    try foo(X)
    of {Y, _} -> 1
    catch
        Class:_:Stack  -> {Class, Stack}
    end.
