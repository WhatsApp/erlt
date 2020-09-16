-module(erlt_defs).

-export([new/0, find_enum/3, find_struct/3, add_definitions/2]).

-export_type([defs/0]).

-type definition_map() :: #{{atom(), atom()} => erl_parse:abstract_form() | private}.

-type exports_map() :: #{atom() => cerl_sets:set({atom(), integer()})}.

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

-spec add_definitions([erl_parse:abstract_form()], defs()) -> defs().
add_definitions(Forms, Defs0) ->
    [Module] = [M || {attribute, _, module, M} <- Forms],
    Exports = gather_exported_types(Forms),
    Defs = Defs0#defs{exported_types = maps:put(Module, Exports, Defs0#defs.exported_types)},
    lists:foldl(fun(Form, Acc) -> add_definition(Form, Module, Acc) end, Defs, Forms).

add_definition({attribute, _Loc, enum, {Name, _Type, Vs}} = Enum, Module, Defs) ->
    add_exported(#defs.enums, Module, Name, length(Vs), Enum, Defs);
add_definition({attribute, _Loc, struct, {Name, _Type, Vs}} = Struct, Module, Defs) ->
    add_exported(#defs.structs, Module, Name, length(Vs), Struct, Defs);
add_definition(_, _, Defs) ->
    Defs.

add_exported(Index, Module, Name, Arity, Value, Defs) ->
    case cerl_sets:is_element({Name, Arity}, map_get(Module, Defs#defs.exported_types)) of
        true ->
            setelement(Index, Defs, maps:put({Module, Name}, Value, element(Index, Defs)));
        false ->
            setelement(Index, Defs, maps:put({Module, Name}, private, element(Index, Defs)))
    end.

gather_exported_types(Forms) ->
    Fun = fun
        ({attribute, _, export_type, List}) -> List;
        (_) -> []
    end,
    cerl_sets:from_list(lists:flatmap(Fun, Forms)).
