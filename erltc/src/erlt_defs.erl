-module(erlt_defs).

-export([new/0, find_enum/3, find_struct/3, add_definitions/2]).

-export_type([defs/0]).

-type definition_map() :: #{{atom(), atom()} => erl_parse:abstract_form()}.

-record(defs, {
    enums = #{} :: definition_map(),
    structs = #{} :: definition_map()
}).

-opaque defs() :: #defs{}.

-spec new() -> defs().
new() -> #defs{}.

-spec find_enum(atom(), atom(), defs()) -> {ok, erl_parse:abstract_form()} | error.
find_enum(Module, Name, #defs{enums = Enums}) ->
    maps:find({Module, Name}, Enums).

-spec find_struct(atom(), atom(), defs()) -> {ok, erl_parse:abstract_form()} | error.
find_struct(Module, Name, #defs{structs = Structs}) ->
    maps:find({Module, Name}, Structs).

-spec add_definitions([erl_parse:abstract_form()], defs()) -> defs().
add_definitions(Forms, Defs) ->
    [Module] = [M || {attribute, _, module, M} <- Forms],
    lists:foldl(fun(Form, Acc) -> add_definition(Form, Module, Acc) end, Defs, Forms).

add_definition({attribute, _Loc, enum, {Name, _Type, _Vs}} = Enum, Module, Defs) ->
    Defs#defs{enums = maps:put({Module, Name}, Enum, Defs#defs.enums)};
add_definition({attribute, _Loc, struct, {Name, _Type, _Vs}} = Struct, Module, Defs) ->
    Defs#defs{structs = maps:put({Module, Name}, Struct, Defs#defs.structs)};
add_definition(_, _, Defs) ->
    Defs.
