-file("doc/src/st_mapping.erlt", 1).

-module(st_mapping).

-export([option_to_list/1,
         rec1/0,
         rec2/0,
         rec3/1,
         rec4/1,
         rec5/1,
         rec6/1,
         rec7/1,
         update_x/2]).

-export_type([float_alias/0,
              some_fun/2,
              list_alias/1,
              tuple0_alias/0,
              tuple1_alias/1,
              tuple2_alias/2,
              tuple3_alias/3,
              atom_alias/0,
              string_alias/0,
              integer_alias/0,
              char_alias/0,
              boolean_alias/0,
              pid_alias/0,
              port_alias/0,
              reference_alias/0,
              neg_integer_alias/0,
              non_neg_integer_alias/0,
              pos_integer_alias/0,
              any_alias/0,
              none_alias/0,
              term_alias/0,
              binary_alias/0,
              bitstring_alias/0,
              byte_alias/0,
              number_alias/0,
              iodata_alias/0,
              iolist_alias/0,
              identifier_alias/0,
              node_alias/0,
              timeout_alias/0,
              no_return_alias/0,
              rec_with_int_id/0,
              rec_with_generic_id/1,
              date/0,
              either/2]).

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

-type rec_with_int_id() :: #{id := integer()}.

-type rec_with_generic_id(A) :: #{id := A}.

-type date() :: #{year := integer(), month := string(),
                  day := integer()}.

-type either(A, B) :: {'$#st_mapping:either.left', A} |
                      {'$#st_mapping:either.right', B}.

-type option(A) :: {'$#st_mapping:option.none'} |
                   {'$#st_mapping:option.some', A}.

-spec option_to_list(option(A)) -> [A].

option_to_list({'$#st_mapping:option.none'}) -> [];
option_to_list({'$#st_mapping:option.some', A}) -> [A].

-spec rec1() -> #{}.

rec1() -> #{}.

-spec rec2() -> date().

rec2() -> #{year => 2020, month => "April", day => 17}.

-spec rec3(#{id := A, atom() => any()}) -> A.

rec3(Rec) -> erlang:map_get(id, Rec).

-spec rec4(date()) -> date().

rec4(Rec) -> Rec#{year => 2046}.

-spec rec5(#{id := A}) -> A.

rec5(Rec) -> erlang:map_get(id, Rec).

-spec rec6(#{year := integer()}) -> #{year :=
                                          integer()}.

rec6(Rec) -> Rec#{year => 2046}.

-spec rec7(#{id := A, atom() => any()}) -> A.

rec7(Rec) -> erlang:map_get(id, Rec).

-spec update_x(#{x := A, atom() => any()}, A) -> #{x :=
                                                       A,
                                                   atom() => any()}.

update_x(R, X) -> R#{x => X}.



