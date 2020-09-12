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
-module(comp).

map(F, L) ->
    [F(X) || X <- L].

filter(F, L) ->
    [X || X <- L, F(X)].

strange_perm(F, L1, L2) ->
    [{X, Y} || X <- L1, Y <- L2, F(X, Y)].

sort([Pivot|T]) ->
    sort([ X || X <- T, X < Pivot])
        ++ [Pivot]
        ++ sort([ Y || Y <- T, Y >= Pivot]);
sort([]) -> [].


sort([Pivot|T], C) ->
    sort([ X || X <- T, C(X, Pivot)], C)
        ++ [Pivot]
        ++ sort([ Y || Y <- T, not C(Y, Pivot)], C);
sort([], _C) -> [].

append(L) ->
    [X || L1 <- L, X <- L1].

flat_map(F, L) ->
    [Y || X <- L, Y <- F(X)].

list_from_binary(Binary) ->
    [ X || <<X>> <= Binary ].

binary_from_list1(List) ->
    << <<X>> || X <- List >>.

binary_from_list2(List) ->
    << X || X <- List >>.

even_from_binary(Binary) ->
    [ X || <<X>> <= Binary, X rem 2 == 0].

rgb(Pixels) ->
    [ {R,G,B} || <<R:8,G:8,B:8>> <= Pixels ].

pixels(RGB) ->
    << <<R:8, G:8, B:8>> ||  {R,G,B} <- RGB >>.

slice(Binary, SliceSize) ->
    [ Slice || <<Slice:SliceSize/binary>> <= Binary].
