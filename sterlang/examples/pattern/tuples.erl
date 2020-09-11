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
-module(tuples).
-export([test1/1, test2/1, test3/1, test4/1, test5/1]).
-export([test6/1, test7/1]).

-spec test1({boolean()}) -> {}.
test1({true}) -> {};
test1({false}) -> {}.

-spec test2({boolean()}) -> {}.
test2(_) -> {}.

-spec test3({boolean()}) -> {}.
test3({_}) -> {}.

-spec test4({boolean()}) -> {}.
test4({true}) -> {};
test4(_) -> {}.

-spec test5({boolean()}) -> {}.
test5({true}) -> {};
test5({_}) -> {}.

-spec test6({boolean(), boolean()}) -> {}.
test6({_, _}) -> {}.

-spec test7({boolean(), boolean()}) -> {}.
test7({true, _}) -> {};
test7({_, _}) -> {}.
