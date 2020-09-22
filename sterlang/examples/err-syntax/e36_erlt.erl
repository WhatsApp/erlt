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

-module(e36_erlt).
-lang(st).

-export([mk_map1/1, mk_map1/2]).

%% OK - the key is an atom
destruct_map1(#(key = Value)) -> Value.

%% Not OK - the key is not an atom
mk_map1(#(Key = Value)) -> {Key, Value}.
