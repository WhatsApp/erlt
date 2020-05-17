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
-module(erl2ocaml_pattern).

-export([dedup_pattern/1, dedup_patterns/1, destruct_pattern/1, destruct_patterns/1]).

dedup_pattern(P) ->
    {P1, Env} = dedup_pattern1(P, []),
    Env1 = lists:map(fun ({V1, V2}) -> {V2, V1} end, Env),
    Env2 = lists:filter(fun ({V1, V2}) -> V1 =/= V2 end, Env1),
    {P1, Env2}.

dedup_patterns(Ps) ->
    {Ps1, Env} = dedup_patterns1(Ps, []),
    Env1 = lists:map(fun ({V1, V2}) -> {V2, V1} end, Env),
    Env2 = lists:filter(fun ({V1, V2}) -> V1 =/= V2 end, Env1),
    {Ps1, Env2}.

dedup_pattern1({var, Line, '_'}, Env) ->
    {{var, Line, '_'}, Env};
dedup_pattern1({var, Line, V}, Env) ->
    V1 =
        case proplists:is_defined(V, Env) of
            true ->
                list_to_atom(
                    "_p'" ++ atom_to_list(V) ++ integer_to_list(erlang:length(Env))
                );
            false ->
                V
        end,
    Env1 = [{V, V1} | Env],
    {{var, Line, V1}, Env1};
dedup_pattern1({match, Line, LP, RP}, Env) ->
    {LP1, Env1} = dedup_pattern1(LP, Env),
    {RP1, Env2} = dedup_pattern1(RP, Env1),
    {{match, Line, LP1, RP1}, Env2};
dedup_pattern1({integer, Line, I}, Env) ->
    {{integer, Line, I}, Env};
dedup_pattern1({char, Line, C}, Env) ->
    {{char, Line, C}, Env};
dedup_pattern1({float, Line, F}, Env) ->
    {{float, Line, F}, Env};
dedup_pattern1({atom, Line, A}, Env) ->
    {{atom, Line, A}, Env};
dedup_pattern1({enum, L1, {op, L2, '.', {atom, L3, Enum}, {atom, L4, Ctr}}, Args}, Env) ->
    {Args1, Env1} = dedup_patterns1(Args, Env),
    {{enum, L1, {op, L2, '.', {atom, L3, Enum}, {atom, L4, Ctr}}, Args1}, Env1};
dedup_pattern1(
    {enum, L1,
        {op, L2, '.', {op, L3, '.', {atom, L4, Mod}, {atom, L5, Enum}}, {atom, L6, Ctr}},
        Args},
    Env
) ->
    {Args1, Env1} = dedup_patterns1(Args, Env),
    {{enum, L1,
        {op, L2, '.', {op, L3, '.', {atom, L4, Mod}, {atom, L5, Enum}}, {atom, L6, Ctr}},
        Args1}, Env1};
dedup_pattern1({string, Line, S}, Env) ->
    {{string, Line, S}, Env};
dedup_pattern1({nil, Line}, Env) ->
    {{nil, Line}, Env};
dedup_pattern1({cons, Line, H, T}, Env) ->
    {H1, Env1} = dedup_pattern1(H, Env),
    {T1, Env2} = dedup_pattern1(T, Env1),
    {{cons, Line, H1, T1}, Env2};
dedup_pattern1({tuple, Line, Ps}, Env) ->
    {Ps1, Env1} = dedup_patterns1(Ps, Env),
    {{tuple, Line, Ps1}, Env1};
dedup_pattern1(Pat, _Map) ->
    Line = erlang:element(2, Pat),
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', Pat}}}).

dedup_patterns1([], Env) ->
    {[], Env};
dedup_patterns1([P | Ps], Env) ->
    {P1, Env1} = dedup_pattern1(P, Env),
    {Ps1, Env2} = dedup_patterns1(Ps, Env1),
    {[P1 | Ps1], Env2}.

destruct_pattern(P) ->
    {P1, Env} = destruct_pattern1(P, []),
    {P1, lists:reverse(Env)}.

destruct_patterns(Ps) ->
    {Ps1, Env} = destruct_patterns1(Ps, []),
    {Ps1, lists:reverse(Env)}.

destruct_pattern1({var, Line, V}, Env) ->
    {{var, Line, V}, Env};
destruct_pattern1({match, Ln, LP, RP}, Env) ->
    {LP1, Env1} = destruct_pattern1(LP, Env),
    {RP1, Env2} = destruct_pattern1(RP, Env1),
    {RP2, Env3} =
        case RP1 of
            {var, _, _} ->
                {RP1, Env2};
            _ ->
                Counter = erlang:get('d_pat'),
                erlang:put('d_pat', Counter + 1),
                FPatVarName = list_to_atom("d_pat'" ++ integer_to_list(Counter)),
                FPatVar = {var, Ln, FPatVarName},
                {FPatVar, [{RP1, FPatVar} | Env2]}
        end,
    {{match, Ln, LP1, RP2}, Env3};
destruct_pattern1({integer, Line, I}, Env) ->
    {{integer, Line, I}, Env};
destruct_pattern1({char, Line, C}, Env) ->
    {{char, Line, C}, Env};
destruct_pattern1({float, Line, F}, Env) ->
    {{float, Line, F}, Env};
destruct_pattern1({atom, Line, A}, Env) ->
    {{atom, Line, A}, Env};
destruct_pattern1(
    {enum, L1, {op, L2, '.', {atom, L3, Enum}, {atom, L4, Ctr}}, Args},
    Env
) ->
    {Args1, Env1} = destruct_patterns1(Args, Env),
    {{enum, L1, {op, L2, '.', {atom, L3, Enum}, {atom, L4, Ctr}}, Args1}, Env1};
destruct_pattern1(
    {enum, L1,
        {op, L2, '.', {op, L3, '.', {atom, L4, Mod}, {atom, L5, Enum}}, {atom, L6, Ctr}},
        Args},
    Env
) ->
    {Args1, Env1} = destruct_patterns1(Args, Env),
    {{enum, L1,
        {op, L2, '.', {op, L3, '.', {atom, L4, Mod}, {atom, L5, Enum}}, {atom, L6, Ctr}},
        Args1}, Env1};
destruct_pattern1({string, Line, S}, Env) ->
    {{string, Line, S}, Env};
destruct_pattern1({nil, Line}, Env) ->
    {{nil, Line}, Env};
destruct_pattern1({cons, Line, H, T}, Env) ->
    {H1, Env1} = destruct_pattern1(H, Env),
    {T1, Env2} = destruct_pattern1(T, Env1),
    {{cons, Line, H1, T1}, Env2};
destruct_pattern1({tuple, Line, Ps}, Env) ->
    {Ps1, Env1} = destruct_patterns1(Ps, Env),
    {{tuple, Line, Ps1}, Env1};
destruct_pattern1(Pat, _Map) ->
    Line = erlang:element(2, Pat),
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', Pat}}}).

destruct_patterns1([], Env) ->
    {[], Env};
destruct_patterns1([P | Ps], Env) ->
    {P1, Env1} = destruct_pattern1(P, Env),
    {Ps1, Env2} = destruct_patterns1(Ps, Env1),
    {[P1 | Ps1], Env2}.
