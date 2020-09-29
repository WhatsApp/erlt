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

-export([module/2, local_rewriter/1]).

-record(context, {
    module :: atom(),
    enums = #{},
    defs_db :: erlt_defs:def_db()
}).

module(Forms, DefDb) ->
    Context = init_context(Forms, DefDb),
    erlt_ast:prewalk(Forms, fun(Node, Ctx) -> rewrite(Node, Context, Ctx) end).

local_rewriter(Forms) ->
    Context = init_context(Forms, erlt_defs:new()),
    fun(Expr) ->
        rewrite_local(Expr, Context)
    end.

init_context(Forms, DefsDb) ->
    [Module] = [M || {attribute, _, module, M} <- Forms],
    Enums = [E || {attribute, _, enum, E} <- Forms],
    #context{
        module = Module,
        enums = init_enums(Enums, Module),
        defs_db = DefsDb
    }.

init_enums(Defs, Module) ->
    Map = [{Name, enum_info(Module, Type)} || {Name, Type, _Args} <- Defs],
    maps:from_list(Map).

enum_info(Mod, {type, _, enum, {atom, _, Name}, Variants}) ->
    VariantsMap = [variant_info(Mod, Name, Variant) || Variant <- Variants],
    maps:from_list(VariantsMap).

variant_info(Mod, Enum, {variant, _, {atom, Line, Tag}, Fields}) ->
    String = "$#" ++ atom_to_list(Mod) ++ ":" ++ atom_to_list(Enum) ++ "." ++ atom_to_list(Tag),
    RuntimeTag = list_to_atom(String),
    Anno = erl_anno:set_generated(true, Line),
    FieldsMap = [
        {Name, Default}
        || {field_definition, _, {atom, _, Name}, Default, _Type} <- Fields
    ],
    {Tag, {{atom, Anno, RuntimeTag}, FieldsMap}}.

rewrite({attribute, Line, enum, {Name, Type, Vars}}, Context, form) ->
    Union = build_type_union(Type, Context),
    {attribute, Line, type, {Name, Union, Vars}};
rewrite({enum, Line, Name, Variant, Fields}, Context, pattern) ->
    {RuntimeTag, Def} = get_definition(Name, Variant, Context),
    Pattern = variant_pattern(Fields, Def),
    {tuple, Line, [RuntimeTag | Pattern]};
rewrite({enum, Line, Name, Variant, Fields}, Context, _Ctx) ->
    {RuntimeTag, Def} = get_definition(Name, Variant, Context),
    Constructor = variant_init(Fields, Def),
    {tuple, Line, [RuntimeTag | Constructor]};
rewrite(Other, _Context, _Ctx) ->
    Other.

rewrite_local({enum, Line, {atom, _, Name}, {atom, _, Variant}, Fields}, Context) ->
    {RuntimeTag, Def} = map_get(Variant, map_get(Name, Context#context.enums)),
    Constructor = variant_init(Fields, Def),
    {tuple, Line, [RuntimeTag | Constructor]}.

get_definition({atom, _, Name}, {atom, _, Variant}, Context) ->
    map_get(Variant, map_get(Name, Context#context.enums));
get_definition({remote, _, {atom, _, Module}, {atom, _, Name}}, {atom, _, Variant}, Context) ->
    map_get(Variant, get_remote_definition(Module, Name, Context)).

get_remote_definition(Module, Name, Context) ->
    {ok, {attribute, _, _, {_, Type, _}}} =
        erlt_defs:find_enum(Module, Name, Context#context.defs_db),
    enum_info(Module, Type).

variant_init(Fields, Defs) ->
    Fun = fun({Name, Default}) ->
        case find_field(Name, Fields) of
            {struct_field, _, _, Value} -> Value;
            error when Default =/= undefined -> Default
        end
    end,
    lists:map(Fun, Defs).

variant_pattern(Fields, Defs) ->
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

build_type_union({type, Line, enum, {atom, _, Name}, Variants}, Context) ->
    Defs = map_get(Name, Context#context.enums),
    {type, Line, union, [variant_type(Variant, Defs) || Variant <- Variants]}.

variant_type({variant, Line, {atom, _, Name}, Fields}, Defs) ->
    {RuntimeTag, _} = map_get(Name, Defs),
    FieldTypes = [Type || {field_definition, _, _Name, _Default, Type} <- Fields],
    {type, Line, tuple, [RuntimeTag | FieldTypes]}.
