-lang([erl2, st]).
-module(etf01).

-export([id/1]).

%% simplest: vars
-spec id(X) -> X.
id(X) -> X.