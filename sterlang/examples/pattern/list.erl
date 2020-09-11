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
-module(list).
-export([test1/1, test2/1, test3/1, test4/1]).

-spec test1(list(boolean())) -> {}.
test1([]) -> {};
test1([_H | _T]) -> {}.

-spec test2(list(boolean())) -> {}.
test2([]) -> {};
test2([true | _T]) -> {};
test2([false | _T]) -> {}.

-spec test3(list(boolean())) -> {}.
test3([]) -> {};
test3([_E]) -> {};
test3([_E1 | [_E2 | _T]]) -> {}.

-spec test4(list(boolean())) -> {}.
test4([]) -> {};
test4([_E | []]) -> {};
test4([_E1 | [_E2 | _T]]) -> {}.
