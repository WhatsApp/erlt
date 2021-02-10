-module(expand).

-export([
    mk_bin_tree/1,
    mk_bin_tree_typo/1,
    tuple_box/1,
    fun_with_rec_constraints/0
]).

-type bin_tree() ::
      {leaf, atom()}
    | {node, bin_tree(), bin_tree()}.

-type box(A) :: {box, A}.

-spec mk_bin_tree(atom()) -> bin_tree().
mk_bin_tree(A) -> {leaf, A}.

-spec mk_bin_tree_typo(atom())
    -> types1:bin_tree_typo().
mk_bin_tree_typo(A) -> {leaf, A}.

-spec tuple_box(Tuple) -> TupleBox when
    Tuple :: {A, B}, TupleBox :: {box(A), box(B)}.
tuple_box({A, B}) -> {{box, A}, {box, B}}.

-spec fun_with_rec_constraints() ->
    Rec when Rec :: {rec, Rec}.
fun_with_rec_constraints() ->
    {rec, fun_with_rec_constraints()}.

-record(rec_box, {box :: box(atom())}).
