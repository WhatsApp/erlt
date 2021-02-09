-module(records).

-compile([export_all, nowarn_export_all]).

-record(rec1, {}).
-record(rec2, {}).
-record(rec3, {}).

-record(any_foo, {id, name}).

-record(foo, {
    id :: integer(),
    name :: atom()
}).

-record(foo_def, {
    id = 1 :: integer(),
    name = default :: atom()
}).

-spec mk_rec1_pos() -> #rec1{}.
mk_rec1_pos() -> #rec1{}.

-spec mk_rec1_neg() -> #rec1{}.
mk_rec1_neg() -> #rec2{}.

-spec mk_rec1a_pos() -> #rec1{}.
mk_rec1a_pos() ->
    R = #rec1{},
    R.

-spec mk_rec1a_neg() -> #rec1{}.
mk_rec1a_neg() ->
    R = #rec2{},
    R.

-spec mk_rec_pos(atom()) ->
    #rec1{} | #rec2{}.
mk_rec_pos(rec1) -> #rec1{};
mk_rec_pos(rec2) -> #rec2{}.

-spec mk_rec_neg(atom()) ->
    #rec1{} | #rec3{}.
mk_rec_neg(rec1) -> #rec1{};
mk_rec_neg(rec2) -> #rec2{}.

-spec mk_foo_pos() -> #foo{}.
mk_foo_pos() ->
    Foo = #foo{id = 42},
    Foo.

-spec mk_foo_neg() -> #foo{}.
mk_foo_neg() ->
    Foo = #foo{name = bar},
    Foo.

-spec fix_foo_pos(#foo{}) -> #foo{}.
fix_foo_pos(Foo) ->
    Foo#foo{name = default, id = 0}.

-spec fix_foo1_pos(#foo{}) -> #foo{}.
fix_foo1_pos(Foo) ->
    Foo1 = Foo#foo{name = default, id = 0},
    Foo1.

-spec fix_foo_neg(#foo{}) -> #foo{}.
fix_foo_neg(Foo) ->
    Foo#foo_def{name = default, id = 0}.

-spec index1_pos() -> integer().
index1_pos() ->
    #foo.name.

-spec index2_pos() -> integer().
index2_pos() ->
    Index = #foo.name,
    Index.

-spec index1_neg() -> atom().
index1_neg() ->
    #foo.name.

-spec index2_neg() -> atom().
index2_neg() ->
    Index = #foo.name,
    Index.

-spec select1_pos(#foo{}) -> integer().
select1_pos(Foo) ->
    Foo#foo.id.

-spec select2_pos(#foo{}) -> integer().
select2_pos(Foo) ->
    Id = Foo#foo.id,
    Id.

-spec select1_neg(#foo{}) -> integer().
select1_neg(Foo) ->
    Foo#foo.name.

-spec select2_neg(#foo{}) -> integer().
select2_neg(Foo) ->
    Id = Foo#foo.name,
    Id.

-spec rec_index_pat_pos(any())
    -> integer().
rec_index_pat_pos(#foo.id = I) -> I.

-spec rec_index_pat_neg(any())
        -> atom().
rec_index_pat_neg(#foo.id = I) -> I.

-spec rec_pat_pos(any())
        -> {integer(), atom()}.
rec_pat_pos(#foo{id = I, name = N}) ->
    {I, N}.

-spec rec_pat_neg(any())
        -> {integer(), atom()}.
rec_pat_neg(#foo{id = I, name = N}) ->
    {N, I}.

-spec pat_fields(#any_foo{}) ->
    {#foo{}, #foo{}}.
pat_fields(
    #any_foo{
        id = (#foo{} = I),
        name = (#foo{} = N)
    }
) -> {I, N}.

-spec rec_guard1(any()) -> #foo{}.
rec_guard1(Foo)
    when Foo#foo.id > 0 -> Foo.

-spec rec_guard2_neg(any()) -> ok.
rec_guard2_neg(Foo)
    when Foo == #foo{} -> ok.

-spec rec_guard3_pos(any(), any())
    -> number().
rec_guard3_pos(Foo, X)
    when Foo == #foo_def{id = X} -> X.

-spec rec_guard4_neg(any(), any())
        -> atom().
rec_guard4_neg(Foo, X)
    when Foo == #foo_def{id = X} -> X.

-record(rec_a, { field :: number() }).
-record(rec_b, { field :: #rec_a{} }).

-spec record_in_record
    (#rec_b{}) -> number().
record_in_record(X) ->
    (X#rec_b.field)#rec_a.field.

-spec record_in_record_neg
    (#rec_b{}) -> atom().
record_in_record_neg(X) ->
    (X#rec_b.field)#rec_a.field.
