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

-module(ma_mod03).

-export([
  test1_global/1,
  test1_local/1,
  test2_global/1,
  test2_local/1,
  test3_global/1,
  test3_local/1,
  test4_global/1,
  test4_local/1,
  test5_global/1,
  test5_local/1,
  test6_global/1,
  test6_local/1
]).

-define(m01, ma_mod01).
-define(m02, ma_mod02).

test1_global(#'ma_mod01:r1'{}) ->
    true.
test1_local(#?m01:r1{}) ->
    true.

test2_global({record, #'ma_mod01:r1'{}}) ->
    true.

test2_local({record, #?m01:r1{}}) ->
    true.

test3_global({record, #'ma_mod01:r1'{field1 = a}}) ->
    true.

test3_local({record, #?m01:r1{field1 = a}}) ->
    true.

test4_global(X) ->
    #'ma_mod01:r1'{field1 = F1, field2 = F2} = X,
    {F1, F2}.

test4_local(X) ->
    #?m01:r1{field1 = F1, field2 = F2} = X,
    {F1, F2}.

test5_global(X) ->
    #'ma_mod02:r21'{field1 = F1, field2 = F2} = X,
    {F1, F2}.

test5_local(X) ->
    #?m02:r21{field1 = F1, field2 = F2} = X,
    {F1, F2}.

test6_global(X) ->
    #'ma_mod02:r22'{field1 = F1, field2 = F2} = X,
    {F1, F2}.

test6_local(X) ->
    #?m02:r22{field1 = F1, field2 = F2} = X,
    {F1, F2}.
