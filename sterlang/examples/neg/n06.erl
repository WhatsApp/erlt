-lang([erl2, st]).
-module(n06).
foo(B, R) ->
    % circularity
    case B of
        true -> R.x;
        false -> R
    end.
