-lang([erl2, st]).

-module(etf19).

open_rec(##{}) -> 1.
closed_rec(#{}) -> 1.
