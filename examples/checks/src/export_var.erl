-lang([erl2, st]).
-module(export_var).

-export([f/1]).

%% Error: use of exported variables is not allowed
f(X) ->
    case X of
        {ok, Y} -> ok;
        {b, Y} -> ok
    end,
    {ok, Y}.
