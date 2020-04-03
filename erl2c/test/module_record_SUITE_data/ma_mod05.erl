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
-module(ma_mod05).

-export([
  test1_global/1,
  test1_local/1,
  test2_global/1,
  test2_local/1
]).

-define(m01, ma_mod01).

test1_global(X) when X#'ma_mod01:r1'.field1 == 1 ->
    true.

test1_local(X) when X#?m01:r1.field1 == 1 ->
    true.

test2_global(X) when X#'ma_mod01:r1'.field1#'ma_mod01:r1'.field2 == 1 ->
    true.

test2_local(X) when X#?m01:r1.field1#?m01:r1.field2 == 1 ->
    true.
