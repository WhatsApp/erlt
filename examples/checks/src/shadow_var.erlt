-lang([erl2, st]).
-module(shadow_var).

-export([f/2]).

%% Error: variable shadowing not allowed even in normal patterns
f(X, Y) ->
    {ok, Y} = X,
    case X of
        {ok, Y} -> true;
        _ -> false
    end.
