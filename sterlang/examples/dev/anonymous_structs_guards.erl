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

% edge cases for structural records
-lang([erl2, st]).
-module(anonymous_structs_guards).

-export([]).

test1(X) ->
    if
        X#(y) -> 1;
        true -> 2
    end.

test2(M) when M#(id) > 0 -> 1;
test2(M) when M#(name)#(first) =/= "" -> 0.


%% create in in guards
test3(M) when M == #(x = 1) -> true.

%% update in guards
test4(M) when M == M#(x = 1) -> true.

%% More updates in guards
test5(M) when M == M#(x = 1)#(y = 2) -> true.
