-module(misc2).
-compile([export_all]).

%%
%%-spec improper_neg() -> any().
%%improper_neg() -> [a | [b | c]].
%%
%%-spec test_env_pos(any()) -> {atom(), atom()}.
%%test_env_pos(X) ->
%%Y = if is_atom(X) -> X end,
%%{X, Y}.
%%
%%-spec test_var_fun_pos(fun((atom()) -> pid()), atom()) -> pid().
%%test_var_fun_pos(F, X) -> F(X).
%%
%%-spec test_var_fun_neg(fun((atom()) -> pid()), reference()) -> pid().
%%test_var_fun_neg(F, X) -> F(X).

-spec catch_pos() -> any().
catch_pos() -> catch lists:map(1, 1).
