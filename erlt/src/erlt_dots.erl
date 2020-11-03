%% Copyright (c) 2020 Facebook, Inc. and its affiliates.
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

-module(erlt_dots).

-export([parse_transform/2]).

-record(context, {
    module :: atom(),
    namespace :: atom()
}).

parse_transform(Forms, _Options) ->
    Context = init_context(Forms),
    erlt_ast:prewalk(Forms, fun(Node, Ctx) -> rewrite(Node, Ctx, Context) end).

init_context(Forms) ->
    [Module] = [M || {attribute, _, module, M} <- Forms],
    Namespace = erlt_parse:concat_dotted(erlt_parse:dotted_butlast(Module)),
    #context{
        module = Module,
        namespace = list_to_atom(Namespace)
    }.

rewrite({op, Line, '.', L, R} = D, pattern, _Context) ->
    %% fold dotted atoms
    D1 = erlt_parse:fold_dots(D),
    case D1 of
        {atom, _, _} ->
            D1;
        % leave to linter
        _ ->
            {op, Line, '.', L, R}
    end;
rewrite({op, Line, '.', L, R} = D, Ctx, _Context) when Ctx =:= guard; Ctx =:= expr ->
    %% fold dotted atoms
    D1 = erlt_parse:fold_dots(D),
    case D1 of
        {atom, _, _} ->
            D1;
        _ ->
            {call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, map_get}}, [R, L]}
    end;
rewrite({call, Line, {op, _L, '.', {atom, _, ''}, {op, L1, '.', M0, F0}}, As0}, expr, _Context) ->
    %% '.'-prefixed call does not get namespace expanded
    {call, Line, {remote, L1, M0, F0}, As0};
rewrite({call, Line, {op, L1, '.', M0, F0}, As0}, expr, Context) ->
    %% dot as module/function separator in call
    case M0 of
        {atom, _, _} ->
            Prefix = Context#context.namespace,
            %% non-dotted module name gets expanded to local namespace
            M1 = {op, L1, '.', {atom, L1, Prefix}, M0},
            {call, Line, {remote, L1, M1, F0}, As0};
        _ ->
            {call, Line, {remote, L1, M0, F0}, As0}
    end;
rewrite({op, Line, '.', L, R} = D, type, _Context) ->
    %% fold dotted atoms
    D1 = erlt_parse:fold_dots(D),
    case D1 of
        {atom, _, _} ->
            D1;
        % leave to linter
        _ ->
            {op, Line, '.', L, R}
    end;
rewrite({remote_type, Line, [M, {atom, Ln, N}, As]}, type, _Context) ->
    %% fold dotted atoms
    M1 =
        case erlt_parse:fold_dots(M) of
            {atom, La, A} ->
                {atom, La, A};
            % leave to linter
            {op, Ld, '.', L, R} ->
                {op, Ld, '.', L, R}
        end,
    {remote_type, Line, [M1, {atom, Ln, N}, As]};
rewrite({user_type, Line, N, As}, type, Context) ->
    case erlt_parse:split_dotted(N) of
        [_] ->
            % plain local type name
            {user_type, Line, N, As};
        [M, T] ->
            %% remote type with plain module part - expand to local namespace
            Prefix = erlt_parse:split_dotted(Context#context.namespace),
            M1 = list_to_atom(erlt_parse:concat_dotted(Prefix ++ [M])),
            T1 = list_to_atom(T),
            {remote_type, Line, [{atom, Line, M1}, {atom, Line, T1}, As]};
        Ns ->
            %% remote type with dotted module part - uses global namespace
            M = list_to_atom(erlt_parse:concat_dotted(lists:droplast(Ns))),
            T = list_to_atom(lists:last(Ns)),
            {remote_type, Line, [{atom, Line, M}, {atom, Line, T}, As]}
    end;
rewrite(Other, _Ctx, _Context) ->
    Other.
