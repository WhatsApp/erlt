-file("doc/src/st_mapping.erlt", 1).

-module(st_mapping).

-type float_alias() :: float().

-type some_fun(A, B) :: fun((A) -> B).

-type list_alias(A) :: [A].

-type tuple2_alias(A, B) :: {A, B}.

-type tuple3_alias(A, B, C) :: {A, B, C}.

-type string_alias() :: string().

-type integer_alias() :: integer().

-type char_alias() :: char().

-type boolean_alias() :: boolean().

-type tuple0_alias() :: {}.

-type tuple1_alias(A) :: {A}.

-type atom_alias() :: atom().

-type pid_alias() :: pid().

-type port_alias() :: port().

-type reference_alias() :: reference().

-type neg_integer_alias() :: neg_integer().

-type non_neg_integer_alias() :: non_neg_integer().

-type pos_integer_alias() :: pos_integer().

-type any_alias() :: any().

-type none_alias() :: none().

-type term_alias() :: term().

-type binary_alias() :: binary().

-type bitstring_alias() :: bitstring().

-type byte_alias() :: byte().

-type number_alias() :: number().

-type iodata_alias() :: iodata().

-type iolist_alias() :: iolist().

-type identifier_alias() :: identifier().

-type node_alias() :: node().

-type timeout_alias() :: timeout().

-type no_return_alias() :: no_return().

-type map_alias(A, B) :: #{A => B}.

-type map_string_int() :: #{string() => integer()}.

-type rec_with_int_id() :: #{id := integer()}.

-type rec_with_generic_id(A) :: #{id := A}.

-type date() :: #{year := integer(), month := string(),
                  day := integer()}.

-type either(A, B) :: {969696,
                       st_mapping,
                       either,
                       left,
                       A} |
                      {969696, st_mapping, either, right, B}.

-type option(A) :: {969696, st_mapping, option, none} |
                   {969696, st_mapping, option, some, A}.

-spec option_to_list(option(A)) -> [A].

option_to_list({969696, st_mapping, option, none}) ->
    [];
option_to_list({969696, st_mapping, option, some, A}) ->
    [A].

rec1() -> #{}.

rec2() -> #{year => 2020, month => "April", day => 17}.

rec3(Rec) -> erlang:map_get(id, Rec).

rec4(Rec) -> Rec#{year := 2046}.

-spec rec5(#{id := A}) -> A.

rec5(Rec) -> erlang:map_get(id, Rec).

-spec rec6(#{year := integer()}) -> #{year :=
                                          integer()}.

rec6(Rec) -> Rec#{year := 2046}.

-spec rec7(#{id := A, _ := _}) -> A.

rec7(Rec) -> erlang:map_get(id, Rec).

update_x(R, X) -> R#{x := X}.



