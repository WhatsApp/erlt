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
-module(multiple_errors).
-export([test1/1, test2/1, test3/1]).

-enum ab() :: a{} | b{}.

-spec test1(ab()) -> {}.
test1(ab.a{}) -> {}.

-spec test2(ab()) -> {}.
test2(ab.b{}) -> {}.

-spec test3(ab()) -> {}.
test3(ab.a{}) -> {};
test3(ab.a{}) -> {}.
