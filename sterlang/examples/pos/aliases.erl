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
-module(aliases).
-export_type([pair/1]).
-export_type([p_con/1]).

-type pair(A) :: {A, A}.

-spec mk_pair1(A, A) -> pair(A).
mk_pair1(X, Y) -> {X, Y}.

mk_pair2(X, Y) -> {X, Y}.

-enum p_con(A) :: con{pair(A)}.

mk_con(X, Y) ->
    p_con.con{{X, X}}.

de_con(p_con.con{X}) -> X.

loop() -> loop().

mk_pair_any1() ->
    {loop(), loop()}.

-spec mk_pair_any2() -> pair(_).
mk_pair_any2() ->
    {loop(), loop()}.
