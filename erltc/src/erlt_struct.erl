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

-module(erlt_struct).

-export([parse_transform/2]).

-record(context, {
    module :: atom(),
    struct = [],
    structs = #{}
}).

parse_transform(Forms, _Options) ->
    Context = init_context(Forms),
    erlt_ast:prewalk(Forms, fun(Node, Ctx) -> rewrite(Node, Ctx, Context) end).

init_context(Forms) ->
    [Module] = [M || {attribute, _, module, M} <- Forms],
    Structs = [Def || {attribute, _, struct, Def} <- Forms],
    #context{
        module = Module,
        structs = init_structs(Structs, Module)
    }.

init_structs(Defs, Module) ->
    Map = [{Tag, struct_info(Module, Tag, Fields)} || {_Name, {type, _, struct, {atom, _, Tag}, Fields}, _Args} <- Defs],
    maps:from_list(Map).

struct_info(Module, Tag, Fields) ->
    RuntimeTag = list_to_atom("$#" ++ atom_to_list(Module) ++ ":" ++ atom_to_list(Tag)),
    Anno = erl_anno:set_generated(true, erl_anno:new(0)),
    FieldsMap = [{Field, _Default = undefined} || {struct_field, _, {atom, _, Field}, _} <- Fields],
    {{atom, Anno, RuntimeTag}, FieldsMap}.

rewrite({attribute, Line, struct, {TypeName, StructType, Args}}, _Ctx, Context) ->
    {type, TypeLine, struct, {atom, _, Tag}, Fields} = StructType,
    {RuntimeTag, _} = map_get(Tag, Context#context.structs),
    Type = {type, TypeLine, tuple, [RuntimeTag | [Type || {struct_field, _, _Name, Type} <- Fields]]},
    {attribute, Line, type, {TypeName, Type, Args}};
rewrite({struct, Line, {atom, _, Name}, Fields}, pattern, Context) ->
    {RuntimeTag, Def} = map_get(Name, Context#context.structs),
    Fields1 = struct_pattern(Fields, Def),
    {tuple, Line, [RuntimeTag | Fields1]};
rewrite({struct, Line, {atom, _, Name}, Fields}, _Ctx, Context) ->
    {RuntimeTag, Def} = map_get(Name, Context#context.structs),
    Fields1 = struct_init(Fields, Def),
    {tuple, Line, [RuntimeTag | Fields1]};
rewrite(Other, _, _) ->
    Other.

struct_init(Fields, Defs) ->
    Fun = fun({Name, Default}) ->
        case find_field(Name, Fields) of
            {struct_field, _, _, Value} -> Value;
            error -> Default
        end
    end,
    lists:map(Fun, Defs).

struct_pattern(Fields, Defs) ->
    Fun = fun({Name, _Default}) ->
        case find_field(Name, Fields) of
            {struct_field, _, _, Value} ->
                Value;
            error ->
                Anno = erl_anno:set_generated(true, erl_anno:new(1)),
                {var, Anno, '_'}
        end
    end,
    lists:map(Fun, Defs).

find_field(Name, [{struct_field, _, {atom, _, Name}, _} = Field | _]) ->
    Field;
find_field(Name, [_ | Rest]) ->
    find_field(Name, Rest);
find_field(_Name, []) ->
    error.
