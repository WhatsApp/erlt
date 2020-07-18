-lang([erl2, st]).
-module(n04).
%% even simpler example of row circularity
foo(B, R) ->
    if
        B -> #{ self => R };
        true -> R
    end.
