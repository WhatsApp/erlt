-lang([erl2, st]).
-module(unqualified_enum).

-enum ab() :: a{} | b{}.

%% Error: enum types outside an enum declaration may not have unqualified constructors
-type c() :: a{} | b{}.
