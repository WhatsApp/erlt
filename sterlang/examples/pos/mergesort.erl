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
-module(mergesort).
split(L, Even, Odd) ->
    case L of
        [] ->
            {Even, Odd};
        [H | T] ->
            case T of
                [] -> {[H | Even], Odd};
                [Ht | Tt] -> split(Tt, [H | Even], [Ht | Odd])
            end
    end.

merge(Lt, L1, L2) ->
    case L1 of
        [] ->
            L2;
        [H1 | T1] ->
            case L2 of
                [] ->
                    L1;
                [H2 | T2] ->
                    if Lt(H1, H2)
                    then [H1 | merge(Lt, T1, L2)]
                    else [H2 | merge(Lt, L1, T2)]
            end
    end.

sort(Lt, L) ->
    case L of
        [] ->
            [];
        [H | T] ->
            case T of
                [] ->
                    L;
                [Ht | Tt] ->
                    {P1, P2} = split(L, [], []),
                    merge(Lt, sort(Lt, P1), sort (Lt, P2))
            end
    end.

mergesort(Lt, L) ->
    sort(Lt, L).

loop(L, N) ->
    case L of
        [] -> N;
        [_ | T] -> loop(T, N+1)
    end.

length(L) ->
    loop(L, 0).

lt(S1, S2) ->
    S1 < S2.

main() ->
    mergesort(fun lt/2, []).