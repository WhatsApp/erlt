-lang([erl2, st]).

-module(etf12).

-export([loop1/1, loop2/1, apply_f/2]).

loop1(X) -> loop1(X).

loop2(X) -> etf12:loop2(X).

apply_f(F, X) -> F(X).