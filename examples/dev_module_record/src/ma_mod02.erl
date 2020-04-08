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
-module(ma_mod02).

-export([
  test1_global/0,
  test1_local/0,
  test21_global/0,
  test21_local/0,
  test22_global/0,
  test22_local/0
]).
-export_type([r22_global/0, r22_local/0, x_global/0, x_local/0]).

-define(m01, ma_mod01).
-define(m02, ma_mod02).

-record(?MODULE:r21, {field1, field2}).

-record(?MODULE:r22, {field1, field2}).

-type r22_global() :: #'ma_mod02:r22'{field1 :: #'ma_mod02:r21'{}}.
-type r22_local() :: #?m02:r22{field1 :: #?m02:r21{}}.

-record(xxx_global, {field1 :: #'ma_mod02:r22'{}}).
-record(xxx_local,  {field1 :: #?m02:r22{}}).

-type x_global() :: #xxx_global{}.
-type x_local()  :: #xxx_local{}.

-spec test1_global() -> #'ma_mod01:r1'{}.
test1_global() ->
    #'ma_mod01:r1'{}.

-spec test1_local() -> #?m01:r1{}.
test1_local() ->
    #?m01:r1{}.

-spec test21_global() -> #'ma_mod02:r21'{}.
test21_global() ->
    #'ma_mod02:r21'{}.

-spec test21_local() -> #?m02:r21{}.
test21_local() ->
    #?m02:r21{}.

-spec test22_global() -> #'ma_mod02:r22'{}.
test22_global() ->
    #'ma_mod02:r22'{}.

-spec test22_local() -> #?m02:r22{}.
test22_local() ->
    #?m02:r22{}.
