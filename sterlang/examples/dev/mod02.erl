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
-module(mod02).

-export([odd/1, id/1, id_caller/1, id_rec/1, even/1, mod01call/1, list_to_string/1, int_to_string/1, mk_unit/0]).

-spec odd(integer()) -> boolean().
odd(X) ->
    even(X - 1).

-spec id(X) -> X.
id(X) ->
    X.

-spec id_caller(X) -> X.
id_caller(X) ->
    id(X).

-spec id_rec(X) -> X.
id_rec(X) ->
    id_rec(X).

-spec even(integer()) -> boolean().
even(0) ->
    true;
even(X) ->
    odd(X - 1).

-spec mod01call(X) -> X.
mod01call(X) ->
    mod01:mod01F(X).

-spec mk_unit() -> integer().
mk_unit() -> 1.
