-lang([erl2, st]).
-module(n02).
%% circularity
f(T) -> f(fun f/1).