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

-lang([erl2]).
-module(ma_mod13).

-compile(export_all).
-compile(nowarn_export_all).

-export_type([r2_global/0, r2_local/0, x_global/0, x_local/0]).

-record(?MODULE:r1, {field1, field2}).
-record(?MODULE:r2, {field1, field2}).
-record(?MODULE:r3, {field1, field2}).

%% from ma_mod_1/ma_mod_2
-spec test1_global() -> #'ma_mod13:r1'{}.
test1_global() ->
    #'ma_mod13:r1'{}.

-spec test1_local() -> #r1{}.
test1_local() ->
    #r1{}.

-type r2_global() :: #'ma_mod13:r2'{field1 :: #'ma_mod13:r1'{}}.
-type r2_local() :: #r2{field1 :: #r1{}}.

-record(xxx_global, {field1 :: #'ma_mod13:r2'{}}).
-record(xxx_local,  {field1 :: #r2{}}).

-type x_global() :: #xxx_global{}.
-type x_local()  :: #xxx_local{}.

%% from ma_mod_3
test2_global(#'ma_mod13:r1'{}) ->
    true.
test2_local(#r1{}) ->
    true.

test3_global({'record', #'ma_mod13:r1'{}}) ->
    true.

test3_local({'record', #r1{}}) ->
    true.

test4_global({'record', #'ma_mod13:r1'{field1 = 'a'}}) ->
    true.

test4_local({'record', #r1{field1 = 'a'}}) ->
    true.

test5_global(X) ->
    #'ma_mod13:r1'{field1 = F1, field2 = F2} = X,
    {F1, F2}.

test5_local(X) ->
    #r1{field1 = F1, field2 = F2} = X,
    {F1, F2}.

%% from ma_mod04
test7_global(X) ->
    X#'ma_mod13:r1'.field1.

test7_local(X) ->
    X#r1.field1.

test8_global(X) ->
    X#'ma_mod13:r1'.field1#'ma_mod13:r1'.field2.

test8_local(X) ->
    X#r1.field1#r1.field2.

%% from ma_mod05
test9_global(X) when X#'ma_mod13:r1'.field1 == 1 ->
    true.

test9_local(X) when X#r1.field1 == 1 ->
    true.

test10_global(X) when X#'ma_mod13:r1'.field1#'ma_mod13:r1'.field2 == 1 ->
    true.

test10_local(X) when X#r1.field1#r1.field2 == 1 ->
    true.

%% from ma_mod_06
test11_global() ->
    #'ma_mod13:r1'.field1.

test11_local() ->
    #r1.field1.

%% from ma_mod_07
test12_global(X) when #'ma_mod13:r1'.field1 == 2 ->
    X.

test12_local(X) when #r1.field1 == 2 ->
    X.

%% from ma_mod_08
test13_global(#'ma_mod13:r1'.field1) ->
    true.

test13_local(#r1.field1) ->
    true.

%% from ma_mod_09
test14_global() ->
    #'ma_mod13:r1'{}.
test14_local() ->
    #r1{}.

test15_global() ->
    {'record', #'ma_mod13:r1'{}}.

test15_local() ->
    {'record', #r1{}}.

test16_global() ->
    {'record', #'ma_mod13:r1'{field1 = 'a'}}.

test16_local() ->
    {'record', #r1{field1 = 'a'}}.

test17_global(F1, F2) ->
    Y = #'ma_mod13:r1'{field1 = F1, field2 = F2},
    Y.

test17_local(F1, F2) ->
    Y = #r1{field1 = F1, field2 = F2},
    Y.

%% from ma_mod_10
test18_global(X) when X == #'ma_mod13:r1'{} ->
    true.
test18_local(X) when X == #r1{} ->
    true.

test19_global(X) when X == {'record', #'ma_mod13:r1'{}} ->
    true.

test19_local(X) when X == {'record', #r1{}} ->
    true.


test20_global(X) when X == {'record', #'ma_mod13:r1'{field1 = 'a'}} ->
    true.

test20_local(X) when X == {'record', #r1{field1 = 'a'}} ->
    true.

%% from ma_mod_11
test21_global() ->
    X = #'ma_mod13:r1'{},
    X#'ma_mod13:r1'{field1 = 'a'}.
test21_local() ->
    X = #r1{},
    X#r1{field1 = 'a'}.

test22_global() ->
    X = #'ma_mod13:r1'{},
    X#'ma_mod13:r1'{field1 = 'a', field2 = 'b'}.
test22_local() ->
    X = #r1{},
    X#r1{field1 = 'a', field2 = 'b'}.

%% from ma_mod_12

-spec test23_global() -> #'ma_mod13:r3'{}.
test23_global() ->
    #'ma_mod13:r3'{}.

-spec test23_local() -> #r3{}.
test23_local() ->
    #r3{}.

