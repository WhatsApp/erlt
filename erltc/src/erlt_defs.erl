-module(erlt_defs).

-export([new/0, find_enum/3, find_struct/3, add_definitions/2, normalise_definitions/1]).

-export_type([defs/0]).

-type definition_map() :: #{{atom(), atom()} => erl_parse:abstract_form() | private}.

-type exports_map() :: cerl_sets:set({atom(), atom()}).

-record(defs, {
    enums = #{} :: definition_map(),
    structs = #{} :: definition_map(),
    exported_types = #{} :: exports_map()
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

-spec add_definitions([erlt_parse:abstract_form()], defs()) -> defs().
add_definitions(Forms, Defs0) ->
    [Module] = [M || {attribute, _, module, M} <- Forms],
    Exports = gather_exported_types(Module, Forms),
    Defs = Defs0#defs{exported_types = maps:merge(Exports, Defs0#defs.exported_types)},
    lists:foldl(fun(Form, Acc) -> add_definition(Form, Module, Acc) end, Defs, Forms).

add_definition({attribute, _Loc, enum, {Name, _Type, _Vs}} = Enum, Module, Defs) ->
    add_exported(#defs.enums, Module, Name, Enum, Defs);
add_definition({attribute, _Loc, struct, {Name, _Type, _Vs}} = Struct, Module, Defs) ->
    add_exported(#defs.structs, Module, Name, Struct, Defs);
add_definition(_, _, Defs) ->
    Defs.

add_exported(Index, Module, Name, Value, Defs) ->
    ExportedTypes = Defs#defs.exported_types,
    case cerl_sets:is_element({Module, Name}, ExportedTypes) of
        true ->
            setelement(Index, Defs, maps:put({Module, Name}, Value, element(Index, Defs)));
        false ->
            setelement(Index, Defs, maps:put({Module, Name}, private, element(Index, Defs)))
    end.

gather_exported_types(Module, Forms) ->
    Fun = fun
        ({attribute, _, export_type, List}) -> [{Module, Name} || {Name, _Arity} <- List];
        (_) -> []
    end,
    cerl_sets:from_list(lists:flatmap(Fun, Forms)).

-spec normalise_definitions([erlt_parse:abstract_form()]) -> [erlt_parse:abstract_form()].
normalise_definitions(Forms) ->
    StructRewriter = erlt_struct:local_rewriter(Forms),
    EnumRewriter = erlt_enum:local_rewriter(Forms),
    [
        normalise_definition(Form, StructRewriter, EnumRewriter)
        || {attribute, _, _, _} = Form <- Forms
    ].

normalise_definition({attribute, Loc, Attr, {Name, Type0, Vs}}, StructRewriter, EnumRewriter) when
    Attr =:= struct; Attr =:= enum
->
    Fun = fun(Node, Ctx) -> normalise_locals(Node, StructRewriter, EnumRewriter, Ctx) end,
    Type = erlt_ast:prewalk(Type0, Fun),
    {attribute, Loc, Attr, {Name, Type, Vs}};
normalise_definition(Other, _StructRewriter, _EnumRewriter) ->
    Other.

%% Expand local structs & enums, so that we don't encounter scoping issues
%% in the importing module
normalise_locals({struct, _, {atom, _, _}, _} = Struct, StructRewriter, _EnumRewriter, guard) ->
    StructRewriter(Struct);
normalise_locals({enum, _, {atom, _, _}, _, _} = Enum, _StructRewriter, EnumRewriter, guard) ->
    EnumRewriter(Enum);
normalise_locals(Other, _StructRewriter, _EnumRewriter, _Ctx) ->
    Other.
