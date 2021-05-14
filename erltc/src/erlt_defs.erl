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

-module(erlt_defs).

-export([
    new/0,
    find_enum/3,
    find_struct/3,
    find_type/3,
    find_function/4,
    add_definitions/2,
    normalise_definitions/1
]).

-export_type([defs/0]).

-type definition_map() :: #{{atom(), atom()} => erl_parse:abstract_form() | private}.

-type type_def_map() :: #{{atom(), atom()} => {ok, arity()} | private}.

-type fun_def_map() :: #{{atom(), atom(), integer()} => checked | unchecked}.

-record(defs, {
    enums = #{} :: definition_map(),
    structs = #{} :: definition_map(),
    types = #{} :: type_def_map(),
    funs = #{} :: fun_def_map()
}).

-opaque defs() :: #defs{}.

-spec new() -> defs().
new() -> #defs{}.

-spec find_enum(atom(), atom(), defs()) -> {ok, erl_parse:abstract_form()} | private | error.
find_enum(Module, Name, #defs{enums = Enums}) ->
    case Enums of
        #{{Module, Name} := private} -> private;
        #{{Module, Name} := Value} -> {ok, Value};
        #{} -> error
    end.

-spec find_struct(atom(), atom(), defs()) -> {ok, erl_parse:abstract_form()} | private | error.
find_struct(Module, Name, #defs{structs = Structs}) ->
    case Structs of
        #{{Module, Name} := private} -> private;
        #{{Module, Name} := Value} -> {ok, Value};
        #{} -> error
    end.

-spec find_type(atom(), atom(), defs()) -> {ok, arity()} | private | error.
find_type(Module, Name, #defs{types = Types}) ->
    case Types of
        #{{Module, Name} := private} -> private;
        #{{Module, Name} := Value} -> {ok, Value};
        #{} -> error
    end.

-spec find_function(atom(), atom(), integer(), defs()) -> checked | unchecked | private | error.
find_function(Module, Name, Arity, #defs{funs = Funs}) ->
    maps:get({Module, Name, Arity}, Funs, error).

-spec add_definitions([erlt_parse:abstract_form()], defs()) -> defs().
add_definitions(Forms, Defs) ->
    [Module] = [M || {attribute, _, module, M} <- Forms],
    ExpT = gather_exported_types(Forms),
    ExpF = gather_exported_functions(Forms),
    lists:foldl(fun(Form, Acc) -> add_definition(Form, Module, ExpT, ExpF, Acc) end, Defs, Forms).

add_definition({attribute, _Loc, enum, {Name, _Type, Vs}} = Enum, Module, ExpT, _, Defs0) ->
    Defs = add_exported(#defs.enums, {Module, Name}, Name, Enum, ExpT, Defs0),
    add_exported(#defs.types, {Module, Name}, Name, length(Vs), ExpT, Defs);
add_definition({attribute, _Loc, struct, {Name, _Type, Vs}} = Struct, Module, ExpT, _, Defs0) ->
    Defs = add_exported(#defs.structs, {Module, Name}, Name, Struct, ExpT, Defs0),
    add_exported(#defs.types, {Module, Name}, Name, length(Vs), ExpT, Defs);
add_definition({attribute, _Loc, type, {Name, _Type, Vs}}, Module, ExpT, _, Defs) ->
    add_exported(#defs.types, {Module, Name}, Name, length(Vs), ExpT, Defs);
add_definition({attribute, _Loc, opaque, {Name, _Type, Vs}}, Module, ExpT, _, Defs) ->
    add_exported(#defs.types, {Module, Name}, Name, length(Vs), ExpT, Defs);
add_definition({attribute, _Loc, unchecked_opaque, {Name, _Type, Vs}}, Module, ExpT, _, Defs) ->
    add_exported(#defs.types, {Module, Name}, Name, length(Vs), ExpT, Defs);
add_definition({function, _Loc, Name, Arity, _}, Module, _, ExpF, Defs) ->
    add_exported(#defs.funs, {Module, Name, Arity}, {Name, Arity}, checked, ExpF, Defs);
add_definition({attribute, _Loc, spec, {{Name, Arity}, _}}, Module, _, ExpF, Defs) ->
    add_exported(#defs.funs, {Module, Name, Arity}, {Name, Arity}, checked, ExpF, Defs);
add_definition({unchecked_function, _Loc, Name, Arity, _}, Module, _, ExpF, Defs) ->
    maybe_add_exported(#defs.funs, {Module, Name, Arity}, {Name, Arity}, unchecked, ExpF, Defs);
add_definition(_, _, _, _, Defs) ->
    Defs.

add_exported(Index, RemoteKey, LocalKey, Value, Exports, Defs) ->
    case sets:is_element(LocalKey, Exports) of
        true ->
            setelement(Index, Defs, maps:put(RemoteKey, Value, element(Index, Defs)));
        false ->
            setelement(Index, Defs, maps:put(RemoteKey, private, element(Index, Defs)))
    end.

maybe_add_exported(Index, RemoteKey, LocalKey, Value, Exports, Defs) ->
    case is_map_key(RemoteKey, element(Index, Defs)) of
        true -> Defs;
        false -> add_exported(Index, RemoteKey, LocalKey, Value, Exports, Defs)
    end.

gather_exported_types(Forms) ->
    Fun = fun
        ({attribute, _, export_type, List}) -> [Name || {Name, _Arity} <- List];
        (_) -> []
    end,
    sets:from_list(lists:flatmap(Fun, Forms)).

gather_exported_functions(Forms) ->
    Fun = fun
        ({attribute, _, export, List}) -> List;
        (_) -> []
    end,
    sets:from_list(lists:flatmap(Fun, Forms)).

-spec normalise_definitions([erlt_parse:abstract_form()]) -> [erlt_parse:abstract_form()].
normalise_definitions(Forms) ->
    StructRewriter = erlt_struct:local_rewriter(Forms),
    EnumRewriter = erlt_enum:local_rewriter(Forms),
    lists:map(fun(Form) -> normalise_form(Form, StructRewriter, EnumRewriter) end, Forms).

normalise_form({attribute, Loc, Attr, {Name, Type0, Vs}}, StructRewriter, EnumRewriter) when
    Attr =:= struct; Attr =:= enum
->
    Fun = fun(Node, Ctx) -> normalise_locals(Node, StructRewriter, EnumRewriter, Ctx) end,
    Type = erlt_ast:prewalk(Type0, Fun),
    {attribute, Loc, Attr, {Name, Type, Vs}};
normalise_form({attribute, _, _, _} = Attr, _, _) ->
    Attr;
normalise_form({function, Loc, Name, Arity, _Clauses}, _, _) ->
    {function, Loc, Name, Arity, []};
normalise_form({unchecked_function, Loc, Name, Arity, _Clauses}, _, _) ->
    {unchecked_function, Loc, Name, Arity, []};
normalise_form(Form, _, _) ->
    Form.

%% Expand local structs & enums, so that we don't encounter scoping issues
%% in the importing module
normalise_locals({struct, _, {atom, _, _}, _} = Struct, StructRewriter, _EnumRewriter, guard) ->
    StructRewriter(Struct);
normalise_locals({enum, _, {atom, _, _}, _, _} = Enum, _StructRewriter, EnumRewriter, guard) ->
    EnumRewriter(Enum);
normalise_locals(Other, _StructRewriter, _EnumRewriter, _Ctx) ->
    Other.
