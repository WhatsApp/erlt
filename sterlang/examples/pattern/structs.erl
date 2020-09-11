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

-lang(st).
-module(structs).
-export([test1/1, test2/1, test3/1, test4/1, test5/1, test6/1, test7/1]).

-struct r0 :: {}.
-struct r1 :: {a :: boolean()}.
-struct r2 :: {a :: boolean(), b :: boolean()}.

-spec test1(#r0{}) -> {}.
test1(_) -> {}.

-spec test2(#r0{}) -> {}.
test2(#r0{}) -> {}.

-spec test3(#r1{}) -> {}.
test3(#r1{}) -> {}.

-spec test4(#r1{}) -> {}.
test4(#r1{a = true}) -> {};
test4(#r1{a = false}) -> {}.

-spec test5(#r1{}) -> {}.
test5(#r1{a = true}) -> {};
test5(#r1{}) -> {}.

-spec test6(#r2{}) -> {}.
test6(#r2{a = true}) -> {};
test6(#r2{b = true}) -> {};
test6(#r2{a = false, b = false}) -> {}.

test7(#r2{a = true}) -> {};
test7(#r2{b = true}) -> {};
test7(#r2{a = false, b = false}) -> {}.
