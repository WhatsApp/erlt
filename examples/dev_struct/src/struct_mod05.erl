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

-lang([erlt]).
-module(struct_mod04).

-export([expr/0, pattern/1, guard/1, field/1, update/1, remote_with_defaults/0]).

-import_type(struct_mod02, [foo/0, bar/0]).

expr() ->
    #foo{}.

pattern(#foo{}) ->
    ok.

guard(Value) when Value =:= #foo{} ->
    ok.

field(Value) when Value#bar.a =:= 1 ->
    Value#bar.b.

update(Value) ->
    Value#bar{a = 2}.

remote_with_defaults() ->
    #struct_mod01:default_with_default{}.
