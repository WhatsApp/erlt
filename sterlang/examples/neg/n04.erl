-lang([erl2, st]).
-module(n04).
%% even simpler example of row circularity
foo(B, R) ->
    if B
    then #{ self => R }
    else R.
