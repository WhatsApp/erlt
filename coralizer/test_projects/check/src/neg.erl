-module(neg).

-compile([export_all, nowarn_export_all]).

%% Gradualizer:
%% https://git.io/JLjXd
-spec foo(A) -> A.
foo(X) -> {X, X}.

-spec atom_id_e(atom()) -> number().
atom_id_e(X) ->
    X.

-spec foo(any(), A) -> A.
foo(X, _) -> X.

-spec match(any()) -> {atom(), atom()}.
match(X) ->
    {ok, _} = X.
