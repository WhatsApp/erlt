%% Copyright Ericsson AB 1996-2020. All Rights Reserved.
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

-module(erlt_enum).

%% The skeleton for this module is erl_id_trans.

%% This module only traverses legal Erlang code. This is most noticeable
%% in guards where only a limited number of expressions are allowed.
%% N.B. if this module is to be used as a basis for transforms then
%% all the error cases must be handled otherwise this module just crashes!

-export([parse_transform/2]).

-record(context, {
    module :: atom(),
    enum = [],
    enums = #{}
}).

-define(ENUM_COOKIE, 969696).

parse_transform(Forms, _Options) ->
    Context = init_context(Forms),
    element(1, erlt_ast:prewalk(Forms, Context, fun rewrite/3)).

init_context(Forms) ->
    [Module] = [M || {attribute, _, module, M} <- Forms],
    Enums = [E || {attribute, _, enum, E} <- Forms],
    #context{
        module = Module,
        enums = init_enums(Enums, #{})
    }.

init_enums([{N, {type, _, enum, {atom, _, C}, _}, _Vs} | Es], Map) ->
    init_enums(Es, add_enum(C, N, Map));
init_enums([{N, {type, _, union, Cs}, _Vs} | Es], Map) ->
    Map1 = lists:foldl(fun (C, M) -> add_enum(C, N, M) end, Map, constrs(Cs)),
    init_enums(Es, Map1);
init_enums([], Map) ->
    Map.

add_enum(C, E, Map) ->
    case maps:find(C, Map) of
        {ok, Enums} ->
            Map#{C := ordsets:add_element(E, Enums)};
        error ->
            #{C => [E]}
    end.

constrs([{type, _, enum, {atom, _, C}, _} | Cs]) ->
    [C | constrs(Cs)];
constrs([]) ->
    [].

rewrite({attribute, Line, enum, {N, T, Vs}}, Context, form) ->
    {{attribute, Line, type, {N, T, Vs}}, Context#context{enum = N}};
rewrite({enum, Line, {remote, _L, M, E}, A, Ps}, Context, _Ctx) ->
    %% remote enum reference Mod.Enum.Constructor{...}
    {{tuple, Line, [{integer, Line, ?ENUM_COOKIE}, M, E, A | Ps]}, Context};
rewrite({enum, Line, E, A, Ps}, Context, _Ctx) ->
    %% local qualified enum reference Enum.Constructor{...}
    M = {atom, Line, Context#context.module},
    {{tuple, Line, [{integer, Line, ?ENUM_COOKIE}, M, E, A | Ps]}, Context};
rewrite({type, Line, enum, {remote, _, M, E}, A, Ts}, Context, type) ->
    %% remote enum reference Mod.Enum.Constructor{...}
    {{type, Line, tuple, [{integer, Line, ?ENUM_COOKIE}, M, E, A | Ts]}, Context};
rewrite({type, Line, enum, E, A, Ts}, Context, type) ->
    %% local qualified enum reference Enum.Constructor{...}
    M = {atom, Line, Context#context.module},
    {{type, Line, tuple, [{integer, Line, ?ENUM_COOKIE}, M, E, A | Ts]}, Context};
rewrite({type, Line, enum, A, Ts}, Context, type) ->
    %% unqualified use can only happen in an enum def, so the
    %% enum name should be given by the context
    E = {atom, Line, Context#context.enum},
    M = {atom, Line, Context#context.module},
    {{type, Line, tuple, [{integer, Line, ?ENUM_COOKIE}, M, E, A | Ts]}, Context};
rewrite(Other, Context, _Ctx) ->
    {Other, Context}.
