-lang([erl2, st]).
-module(tuple).
-compile(export_all).

-spec pair(A, B) -> {A, B}.
pair(A, B) -> {A, B}.

-spec pair(A) -> fun((B) -> {A, B}).
pair(A) -> fun(B) -> pair(A, B) end.

-spec first({A, _}) -> A.
first({A, _}) -> A.

-spec second({_, B}) -> B.
second({_, B}) -> B.

-spec map_first(fun((A) -> X), {A, B}) -> {X, B}.
map_first(F, {A, B}) -> {F(A),B}.

-spec map_first(fun((A) -> X)) -> fun(({A, B}) -> {X, B}).
map_first(F) -> fun(P) -> map_first(F, P) end.

-spec map_second(fun((B) -> Y), {A, B}) -> {A, Y}.
map_second(F, {A, B}) -> {A,F(B)}.

-spec map_second(fun((B) -> Y)) -> fun(({A, B}) -> {A, Y}).
map_second(F) -> fun(P) -> map_second(F, P) end.

-spec map_both(fun((A) -> X), fun((B) -> Y), {A, B}) -> {X, Y}.
map_both(Fa, Fb, {A, B}) -> {Fa(A), Fb(B)}.

-spec map_both(fun((A) -> X), fun((B) -> Y)) -> fun(({A, B}) -> {X, Y}).
map_both(Fa, Fb) -> fun(P) -> map_both(Fa, Fb, P) end.
