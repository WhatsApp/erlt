-lang([erl2, st]).
-module(n04).
%% even simpler example of row circularity
foo(B, R) ->
    case B of
        true -> #{ self => R };
        false -> R
    end.
