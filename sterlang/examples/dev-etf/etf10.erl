-lang([erl2, st]).

-module(etf10).

-export([block/2]).

-spec block(A, [A]) -> {[A], [A]}.
block(X, Y) ->
    {
        begin
            Z = [X | Y],
            Z
        end,
        begin
            [X | Y]
        end
    }.
