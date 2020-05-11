-lang([erl2, st]).
-module(n06).
foo(B, R) ->
    % circularity
    if B then R.x else R.
