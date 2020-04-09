-lang([erl2, st]).
-module(unspeced_fun).

-export([id/1]).

% Error: exported functions should have specs.
id(X) -> X.
