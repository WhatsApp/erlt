-lang([erl2, st]).
-module(bad_caret).

-export([f/1, g/1, h/1]).

%% Error: caret-marked variables cannot be used in normal expressions
f(X) ->
    {ok, ^X}.

%% Error: caret-marked variables cannot be used in guard expressions
g(X) ->
    case X of
        {'foo', Y} when Y =:= ^X -> ok;
        _ -> error
    end.

%% Error: caret prefix can only be used on variables
h(X) ->
    case X of
        {^'foo', _} -> ok;
        {'foo', ^_} -> ok;
        _ -> X + ^42
    end.
