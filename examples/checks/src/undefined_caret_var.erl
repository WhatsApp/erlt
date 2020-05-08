-lang([erl2, st]).
-module(undefined_caret_var).

-export([f/1, g/1, h/1]).

%% Error: referring to an unknown variable in a function header
f(^X) ->
    ok.

%% Error: referring to an unknown variable in a match pattern
g(X) ->
    {foo, ^Q} = X.

%% Error: referring to an unknown variable in a case clause pattern
h(X) ->
    case X of
        {foo, ^Q} -> ok;
        _ -> error
    end.