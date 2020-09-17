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
-module(struct_mod01).

-export_type([foo/0, bar/1, default_with_default/0]).

-export([foo/0, bar/0, baz/0, pattern/2, guard/1, field/1, update/1]).

-struct foo :: ().

-struct bar(A) :: (a :: A, b :: foo()).

-struct baz :: (bar = #bar{a = trunc(1.5), b = #foo{}} :: bar(integer())).

-struct default_with_default :: (x = #baz{} :: baz()).

foo() ->
    #foo{}.

bar() ->
    #bar{a = 1, b = #foo{}}.

baz() ->
    {#baz{}, #default_with_default{}}.

pattern(#foo{}, #bar{b = B}) -> B.

guard(Value) when Value =:= #bar{a = 1, b = 2} ->
    ok.

field(Value) when Value#bar.a =:= 1 ->
    Value#bar.b.

update(Value) ->
    Value#bar{a = 2}.
