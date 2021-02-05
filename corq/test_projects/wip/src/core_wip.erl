-module(core_wip).
-compile([export_all]).

-type ty1(X) :: [X] | pid() | reference() | file:filename().
-opaque ty2(X, Y) :: fun((X) -> Y).

-export_type([ty1/1, ty2/2]).

-record(rec, {a :: number(), b :: string()}).

-spec test_01() -> #{}.
test_01() ->
  #{}.

-spec test_02() -> #{a => 1, b => 2, c := 3 }.
test_02() ->
  #{a => 1, c => 2}.

-spec test_03() -> #rec{}.
test_03() -> #rec{a = 3}.

-spec test_fun_neg() -> list(any()).
test_fun_neg() -> fun(X) -> X end.

-spec map_pattern(any(), any()) -> ok.
map_pattern(M, K) ->
  #{ K := V1, K := V2 } = M,
  K, V1, K, V2.

-spec test_fun_pos(atom()) -> number().
test_fun_pos(Atom) ->
  30 * (fun(AnAtom, ANum) -> lists:nth(0, atom_to_list(AnAtom)) * ANum end)(Atom, 20).

