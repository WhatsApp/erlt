-module(tries).

-compile([export_all, nowarn_export_all]).

-spec foo() -> {atom(), any()}.
foo() -> {foo, bar}.

-spec ok() -> ok.
ok() -> ok.

-spec only_atom(atom()) -> ok.
only_atom(A) when is_atom(A) -> ok.

-spec test01_pos() -> atom().
test01_pos() ->
    try foo() of
        {X, _} -> X
    catch
        {error, _} -> error
    end.

-spec test02_neg() -> atom().
test02_neg() ->
    try foo() of
        {_, Y} -> Y
    catch
        {error, _} -> error
    end.

-spec test03_neg() -> atom().
test03_neg() ->
    Res = try foo() of
        {_, _} -> []
    catch
        {error, _} -> error
    end,
    Res.

-spec test04_pos(any()) ->
    {ok | error, atom()}.
test04_pos(X) ->
    Res = try foo() of
              {_, _} -> ok
          catch
              {error, _} -> error
          after
            if
                is_atom(X) -> ok
            end
          end,
    {Res, X}.

-spec test05_pos(any()) ->
    {ok | error, atom()}.
test05_pos(X) ->
    A =
        try ok()
        catch
            {error, _} -> error
        after
            if
                is_atom(X) -> ok
            end
        end,
    {A, X}.

-spec test06_neg() -> ok.
test06_neg() ->
    try ok()
    catch _ -> error
    end.

-spec test07_neg() -> error.
test07_neg() ->
    try ok()
    catch _ -> error
    end.

-spec test08_neg() -> ok.
test08_neg() ->
    try ok()
    after
        only_atom(1)
    end.

-spec test09_pos() -> ok.
test09_pos() ->
    try ok()
    after
        only_atom(atom)
    end.

-spec test10_pos() -> ok.
test10_pos() ->
    try ok() of
        X -> X
    after
        only_atom(atom)
    end.
