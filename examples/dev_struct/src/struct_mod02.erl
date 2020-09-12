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
-module(struct_mod02).

-export_type([foo/0]).

-export([expr/0, pattern/1, guard/1]).

-struct foo :: ().

expr() ->
    #?MODULE:foo{}.

pattern(#?MODULE:foo{}) ->
    ok.

guard(Value) when Value =:= #?MODULE:foo{} ->
    ok.
