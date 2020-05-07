%% Copyright (c) Facebook, Inc. and its affiliates.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-lang([erl2, st]).
-module(st_mapping).

%% 1:1 mapping of basic types:
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

%% opaque mapping of "builtin" types
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
%% this is not supported for now
%%-type maybe_improper_list_alias() :: maybe_improper_list().
%%-type nonempty_list_alias() :: nonempty_list().
%%-type nonempty_string_alias() :: nonempty_string().
-type iodata_alias() :: iodata().
-type iolist_alias() :: iolist().
-type identifier_alias() :: identifier().
-type node_alias() :: node().
-type timeout_alias() :: timeout().
-type no_return_alias() :: no_return().

%% opaque mapping of homogeneous maps
-type map_alias(A, B) :: #{A => B}.
-type map_string_int() :: #{string() => integer()}.

%% mapping of polymorphic records
-type rec_with_int_id() :: #{'id' := integer()}.
-type rec_with_generic_id(A) :: #{'id' := A}.
-type date() :: #{'year' := integer(), 'month' := string(), 'day' := integer()}.

%% Enums
-enum either(A, B) :: left{A} | right {B}.
-enum option(A) :: none{} | some{A}.

-spec option_to_list(option(A)) -> [A].
option_to_list(option.none{}) -> [];
option_to_list(option.some{A}) -> [A].

%% Records

rec1() ->
    #{}.

rec2() ->
    #{'year' => 2020, 'month' => "April", 'day' => 17}.

rec3(Rec) ->
    Rec.id.

rec4(Rec) ->
    Rec#{'year' := 2046}.

-spec rec5(#{'id' := A}) -> A.
rec5(Rec) ->
    Rec.id.

-spec rec6(#{'year' := integer()}) -> #{'year' := integer()}.
rec6(Rec) ->
    Rec#{'year' := 2046}.

-spec rec7(#{'id' := A, _ := _}) -> A.
rec7(Rec) ->
    Rec.id.

%%-spec rec8(#{year := integer(), _ := _}) -> #{year := integer(), _ := _}.
%%rec8(Rec) ->
%%    Rec#{year := 2046}.

update_x(R, X) ->
    R#{ 'x' := X }.
