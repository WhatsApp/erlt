-lang([erl2, st]).
-module(redefine_enum).

-enum foo() :: a{}.

%% Error: enum type names may not be reused, even for different arities
-enum foo(T) :: b{T}.
