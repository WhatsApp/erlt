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
-module(ma_mod09).

-export([
  test1_global/0,
  test1_local/0,
  test2_global/0,
  test2_local/0,
  test3_global/0,
  test3_local/0,
  test4_global/2,
  test4_local/2
]).

-define(m01, ma_mod01).


test1_global() ->
    #'ma_mod01:r1'{}.
test1_local() ->
    #?m01:r1{}.

test2_global() ->
    {'record', #'ma_mod01:r1'{}}.

test2_local() ->
    {'record', #?m01:r1{}}.

test3_global() ->
    {'record', #'ma_mod01:r1'{field1 = 'a'}}.

test3_local() ->
    {'record', #?m01:r1{field1 = 'a'}}.

test4_global(F1, F2) ->
    Y = #'ma_mod01:r1'{field1 = F1, field2 = F2},
    Y.

test4_local(F1, F2) ->
    Y = #?m01:r1{field1 = F1, field2 = F2},
    Y.
