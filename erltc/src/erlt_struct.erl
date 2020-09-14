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

-export([module/2]).

-record(context, {
    module :: atom(),
    structs = #{},
    def_db :: erlt_defs:def_db()
}).

-define(CALL(Anno, Mod, Fun, Args),
    {call, Anno, {remote, Anno, {atom, Anno, Mod}, {atom, Anno, Mod}}, Args}
).

module(Forms, DefDb) ->
    Context = init_context(Forms, DefDb),
    put(struct_gen_var, 0),
    try
        erlt_ast:prewalk(Forms, fun(Node, Ctx) -> rewrite(Node, Ctx, Context) end)
    after
        erase(struct_gen_var)
    end.

init_context(Forms, DefDb) ->
    [Module] = [M || {attribute, _, module, M} <- Forms],
    Structs = [Def || {attribute, _, struct, Def} <- Forms],
    #context{
        module = Module,
        structs = init_structs(Structs, Module),
        def_db = DefDb
    }.

init_structs(Defs, Module) ->
    Map = [{Name, struct_info(Module, Type)} || {Name, Type, _Args} <- Defs],
    maps:from_list(Map).

struct_info(Module, {type, _, struct, {atom, _, Tag}, Fields}) ->
    RuntimeTag = list_to_atom("$#" ++ atom_to_list(Module) ++ ":" ++ atom_to_list(Tag)),
    Anno = erl_anno:set_generated(true, erl_anno:new(0)),
    FieldsMap = [{Field, _Default = undefined} || {struct_field, _, {atom, _, Field}, _} <- Fields],
    {{atom, Anno, RuntimeTag}, FieldsMap}.

rewrite({attribute, Line, struct, {TypeName, StructType, Args}}, _Ctx, Context) ->
    {type, TypeLine, struct, {atom, _, Tag}, Fields} = StructType,
    {RuntimeTag, _} = map_get(Tag, Context#context.structs),
    Type =
        {type, TypeLine, tuple, [RuntimeTag | [Type || {struct_field, _, _Name, Type} <- Fields]]},
    {attribute, Line, type, {TypeName, Type, Args}};
rewrite({struct, Line, Name, Fields}, pattern, Context) ->
    {RuntimeTag, Def} = get_definition(Name, Context),
    Fields1 = struct_pattern(Fields, Def),
    {tuple, Line, [RuntimeTag | Fields1]};
rewrite({struct, Line, Name, Fields}, _Ctx, Context) ->
    {RuntimeTag, Def} = get_definition(Name, Context),
    Fields1 = struct_init(Fields, Def),
    {tuple, Line, [RuntimeTag | Fields1]};
rewrite({struct_field, Line, Expr, Name, Field}, Ctx, Context) ->
    {RuntimeTag, Def} = get_definition(Name, Context),
    struct_field(Line, Expr, RuntimeTag, Def, Field, Ctx);
rewrite(Other, _, _) ->
    Other.

get_definition({atom, _, Name}, Context) ->
    map_get(Name, Context#context.structs);
get_definition({remote, _, {atom, _, Module}, {atom, _, Name}}, Context) ->
    {ok, {attribute, _, _, {_, Type, _}}} =
        erlt_defs:find_struct(Module, Name, Context#context.def_db),
    struct_info(Module, Type).

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

struct_field(Line, Expr, RuntimeTag, Def, {atom, _, FieldRaw} = Field, expr) ->
    Var = gen_var(Line, FieldRaw),
    GenLine = erl_anno:set_generated(true, Line),
    Pattern = struct_pattern([{struct_field, GenLine, Field, Var}], Def),
    Error = {tuple, GenLine, [{atom, GenLine, badstruct}, RuntimeTag]},
    {'case', GenLine, Expr, [
        {clause, GenLine, [{tuple, GenLine, [RuntimeTag | Pattern]}], [], [Var]},
        {clause, GenLine, [{var, GenLine, '_'}], [], [?CALL(GenLine, erlang, error, [Error])]}
    ]}.

gen_var(Line, Mnemo) ->
    Num = get(struct_gen_var),
    put(struct_gen_var, Num + 1),
    Name = "StructGenVar@" ++ integer_to_list(Num) ++ "@" ++ atom_to_list(Mnemo),
    {var, Line, list_to_atom(Name)}.
