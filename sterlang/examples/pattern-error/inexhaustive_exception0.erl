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
-module(inexhaustive_exception0).
-export([test/1]).

-exception #e0{}.
-exception #e1{a :: boolean()}.

%% We require a catch all case when matching on exceptions as data
-spec test(exception()) -> {}.
test(#e0{}) -> {};
test(#e1{a = true}) -> {};
test(#e1{a = false}) -> {}.
