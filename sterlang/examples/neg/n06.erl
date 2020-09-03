-lang([erl2, st]).
-module(n06).
foo(B, R) ->
    % circularity
    if B -> R#(x); true -> R end.
