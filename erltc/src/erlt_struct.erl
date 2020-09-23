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

-export([module/2, local_rewriter/1]).

-record(context, {
    module :: atom(),
    structs = #{},
    def_db :: erlt_defs:def_db(),
    imported :: #{atom() => atom()},
    extra_guard_checks = []
}).

-define(CALL(Anno, Mod, Fun, Args),
    {call, Anno, {remote, Anno, {atom, Anno, Mod}, {atom, Anno, Fun}}, Args}
).

module(Forms, DefDb) ->
    Context = init_context(Forms, DefDb),
    put(struct_gen_var, 0),
    try
        element(1, erlt_ast:traverse(Forms, Context, fun rewrite/3, fun post/3))
    after
        erase(struct_gen_var)
    end.

local_rewriter(Forms) ->
    Context = init_context(Forms, erlt_defs:new()),
    fun(Expr) ->
        %% there shouldn't be any var generation here, crash otherwise
        put(struct_gen_var, please_crash_if_used),
        try
            rewrite_local(Expr, Context)
        after
            erase(struct_gen_var)
        end
    end.

init_context(Forms, DefDb) ->
    Res = [M || {attribute, _, module, M} <- Forms],
    [Module] = Res,
    Structs = [Def || {attribute, _, struct, Def} <- Forms],
    #context{
        module = Module,
        structs = init_structs(Structs, Module),
        imported = imported(Forms),
        def_db = DefDb
    }.

init_structs(Defs, Module) ->
    Map = [{Name, struct_info(Module, Type)} || {Name, Type, _Args} <- Defs],
    maps:from_list(Map).

struct_info(Module, {type, _, struct, {atom, _, Tag}, Fields}) ->
    RuntimeTag = list_to_atom("$#" ++ atom_to_list(Module) ++ ":" ++ atom_to_list(Tag)),
    Anno = erl_anno:set_generated(true, erl_anno:new(0)),
    FieldsMap = [
        {Name, Default}
        || {field_definition, _, {atom, _, Name}, Default, _Type} <- Fields
    ],
    {{atom, Anno, RuntimeTag}, FieldsMap}.

imported(Forms) ->
    Fun = fun
        ({attribute, _, import_type, {Mod, ImportList}}) ->
            [{Name, Mod} || {Name, _Arity} <- ImportList];
        (_) ->
            []
    end,
    Imports = lists:flatmap(Fun, Forms),
    maps:from_list(Imports).

rewrite({attribute, Line, struct, {TypeName, StructType, Args}}, Context, _Ctx) ->
    {type, TypeLine, struct, {atom, _, Tag}, Fields} = StructType,
    {RuntimeTag, _} = map_get(Tag, Context#context.structs),
    FieldTypes = [Type || {field_definition, _, _Name, _Default, Type} <- Fields],
    Type = {type, TypeLine, tuple, [RuntimeTag | FieldTypes]},
    {{attribute, Line, type, {TypeName, Type, Args}}, Context};
rewrite({struct, Line, Name, Fields}, Context, pattern) ->
    {RuntimeTag, Def} = get_definition(Name, Context),
    Pattern = struct_pattern(Fields, Def),
    {{tuple, Line, [RuntimeTag | Pattern]}, Context};
rewrite({struct, Line, Name, Fields}, Context, _Ctx) ->
    {RuntimeTag, Def} = get_definition(Name, Context),
    Constructor = struct_init(Fields, Def),
    {{tuple, Line, [RuntimeTag | Constructor]}, Context};
rewrite({struct, Line, Expr, Name, Fields}, Context, _Ctx) ->
    {RuntimeTag, Def} = get_definition(Name, Context),
    {struct_update(Line, Expr, RuntimeTag, Def, Fields), Context};
rewrite({struct_field, Line, Expr, Name, Field}, Context, expr) ->
    {RuntimeTag, Def} = get_definition(Name, Context),
    {struct_field_expr(Line, Expr, RuntimeTag, Def, Field), Context};
rewrite({struct_field, Line, Expr, Name, Field}, Context, guard) ->
    {RuntimeTag, Def} = get_definition(Name, Context),
    Check = struct_field_guard_check(Line, Expr, RuntimeTag, Def),
    Context1 = Context#context{extra_guard_checks = [Check | Context#context.extra_guard_checks]},
    {struct_field_guard(Line, Expr, Def, Field), Context1};
rewrite({struct_index, Line, Name, {atom, _, Field}}, Context, _) ->
    {_RuntimeTag, Def} = get_definition(Name, Context),
    {{integer, Line, find_index(Field, Def, 2)}, Context};
rewrite(Other, Context, _) ->
    {Other, Context}.

rewrite_local({struct, Line, {atom, _, Name} = FullName, Fields}, Context) ->
    case maps:find(Name, Context#context.imported) of
        {ok, Module} ->
            {struct, Line, {remote, Line, {atom, Line, Module}, FullName}, Fields};
        error ->
            {RuntimeTag, Def} = map_get(Name, Context#context.structs),
            Constructor = struct_init(Fields, Def),
            {tuple, Line, [RuntimeTag | Constructor]}
    end.

post({guard_and, Line, Exprs}, Context, _) ->
    Context1 = Context#context{extra_guard_checks = []},
    {{guard_and, Line, Context#context.extra_guard_checks ++ Exprs}, Context1};
post(Other, Context, _) ->
    {Other, Context}.

get_definition({atom, _, Name}, Context) ->
    case maps:find(Name, Context#context.imported) of
        {ok, Module} -> get_remote_definition(Module, Name, Context);
        error -> map_get(Name, Context#context.structs)
    end;
get_definition({remote, _, {atom, _, Module}, {atom, _, Name}}, Context) ->
    get_remote_definition(Module, Name, Context).

get_remote_definition(Module, Name, Context) ->
    {ok, {attribute, _, _, {_, Type, _}}} =
        erlt_defs:find_struct(Module, Name, Context#context.def_db),
    struct_info(Module, Type).

struct_init(Fields, Defs) ->
    Fun = fun({Name, Default}) ->
        case find_field(Name, Fields) of
            {struct_field, _, _, Value} -> Value;
            error when Default =/= undefined -> Default
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

update_pattern(Line, Fields, Defs) ->
    Fun = fun({Name, _Default}) ->
        case find_field(Name, Fields) of
            {struct_field, _, _, _} ->
                {{var, Line, '_'}, []};
            error ->
                Var = gen_var(Line, Name),
                {Var, [{struct_field, Line, {atom, Line, Name}, Var}]}
        end
    end,
    {Pattern, ExtraFields} = lists:unzip(lists:map(Fun, Defs)),
    {Pattern, lists:flatten(ExtraFields)}.

find_field(Name, [{struct_field, _, {atom, _, Name}, _} = Field | _]) ->
    Field;
find_field(Name, [_ | Rest]) ->
    find_field(Name, Rest);
find_field(_Name, []) ->
    error.

find_index(Name, [{Name, _} | _Rest], Acc) ->
    Acc;
find_index(Name, [_ | Rest], Acc) ->
    find_index(Name, Rest, Acc + 1).

struct_field_expr(Line, Expr, RuntimeTag, Def, {atom, _, FieldRaw} = Field) ->
    Var = gen_var(Line, FieldRaw),
    GenLine = erl_anno:set_generated(true, Line),
    Pattern = struct_pattern([{struct_field, GenLine, Field, Var}], Def),
    {'case', GenLine, Expr, [
        {clause, GenLine, [{tuple, GenLine, [RuntimeTag | Pattern]}], [], [Var]},
        {clause, GenLine, [{var, GenLine, '_'}], [], [badstruct(GenLine, RuntimeTag)]}
    ]}.

struct_field_guard(Line, Expr, Def, {atom, _, FieldRaw}) ->
    GenLine = erl_anno:set_generated(true, Line),
    Index = {integer, GenLine, find_index(FieldRaw, Def, 2)},
    ?CALL(GenLine, erlang, element, [Index, Expr]).

struct_field_guard_check(Line, Expr, RuntimeTag, Def) ->
    GenLine = erl_anno:set_generated(true, Line),
    Check = ?CALL(GenLine, erlang, is_record, [
        Expr,
        RuntimeTag,
        {integer, GenLine, length(Def) + 1}
    ]),
    %% Force guard crash by evaluating to non-boolean
    {op, GenLine, 'orelse', Check, {atom, GenLine, fail}}.

struct_update(Line, Expr, RuntimeTag, Def, Fields) ->
    GenLine = erl_anno:set_generated(true, Line),
    {Pattern, ExtraFields} = update_pattern(GenLine, Fields, Def),
    Constructor = struct_init(Fields ++ ExtraFields, Def),
    Value = {tuple, Line, [RuntimeTag | Constructor]},
    {'case', GenLine, Expr, [
        {clause, GenLine, [{tuple, GenLine, [RuntimeTag | Pattern]}], [], [Value]},
        {clause, GenLine, [{var, GenLine, '_'}], [], [badstruct(GenLine, RuntimeTag)]}
    ]}.

badstruct(Line, Tag) ->
    ?CALL(Line, erlang, error, [{tuple, Line, [{atom, Line, badstruct}, Tag]}]).

gen_var(Line, Mnemo) ->
    Num = get(struct_gen_var),
    put(struct_gen_var, Num + 1),
    Name = "StructGenVar@" ++ integer_to_list(Num) ++ "@" ++ atom_to_list(Mnemo),
    {var, Line, list_to_atom(Name)}.
