-lang([erl2, st]).
-module(illegal_enum).

-enum one() :: a{}.

%% Error: an enum declaration must only list unqualified constructors
-enum two() :: b{} | one.a{}.
