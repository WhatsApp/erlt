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
-module(struct_mod02).

-export_type([foo/0, bar/0]).

-export([expr/0, pattern/3, guard/3, field/1, update/1, index/1]).

-struct foo :: ().

-struct bar :: (a :: integer(), b :: integer()).

expr() ->
    {
        #?MODULE:foo{},
        #?MODULE:bar{a = 1, b = 2}
    }.

pattern(#?MODULE:foo{}, #?MODULE:bar{a = 1, b = B}, B) ->
    B.

guard(Value1, Value2, B) when Value1 =:= #?MODULE:foo{}, Value2 =:= #?MODULE:bar{a = 1, b = B} ->
    ok.

field(Value) when Value#?MODULE:bar.a =:= 1 ->
    Value#?MODULE:bar.b.

update(Value) ->
    Value#?MODULE:bar{a = 2}.

index(#?MODULE:bar.a) when #?MODULE:bar.a =:= 2 ->
    #?MODULE:bar.b.
