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
-module(records).
-export([test0/1, test1/1, test2/1, test3/1, test4/1, test5/1, test6/1, test7/1, test8/1]).

-spec test0(#{}) -> {}.
test0(_) -> {}.

-spec test1(#{}) -> {}.
test1(#{}) -> {}.

-spec test2(#{}) -> {}.
test2(##{}) -> {}.

-spec test3(#{a := boolean()}) -> {}.
test3(#{a := true}) -> {};
test3(#{a := false}) -> {}.

-spec test4(#{a := boolean()}) -> {}.
test4(##{a := true}) -> {};
test4(##{a := false}) -> {}.

-spec test5(#{a := boolean()}) -> {}.
test5(##{a := true}) -> {};
test5(##{}) -> {}.

-spec test6(#{a := boolean()}) -> {}.
test6(##{}) -> {}.

-spec test7(#{a := boolean(), b := boolean()}) -> {}.
test7(##{a := true}) -> {};
test7(##{b := true}) -> {};
test7(##{a := false, b := false}) -> {}.

test8(##{a := true}) -> {};
test8(##{b := true}) -> {};
test8(##{a := false, b := false}) -> {}.
