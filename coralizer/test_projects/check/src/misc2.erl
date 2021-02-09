-module(misc2).
-compile([export_all]).

-spec improper_neg() -> any().
improper_neg() -> [a | [b | c]].

-spec test_env_pos(any()) -> {atom(), atom()}.
test_env_pos(X) ->
  Y = if is_atom(X) -> X end,
  {X, Y}.

-spec test_var_fun_pos(fun((atom()) -> pid()), atom()) -> pid().
test_var_fun_pos(F, X) -> F(X).

-spec test_var_fun_neg(fun((atom()) -> pid()), reference()) -> pid().
test_var_fun_neg(F, X) -> F(X).

-spec atom_call_neg_01(atom()) -> any().
atom_call_neg_01(X) -> X:main(1).

-spec atom_call_neg_02(atom()) -> any().
atom_call_neg_02(X) -> lists:X(1).

-spec atom_call_neg_03(maps | sets, new) -> any().
atom_call_neg_03(M, F) -> M:F(0).

-spec atom_call_neg_04(maps, to_list | union) -> any().
atom_call_neg_04(M, F) -> M:F(#{}).

-spec atom_call_pos_01(lists, member) -> boolean().
atom_call_pos_01(Mod, Fun) -> Mod:Fun(1, []).


