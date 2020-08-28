-module(erlt_defs).

-export([add_definitions/2]).

-type definition_map() :: #{{atom(), atom()} => erl_parse:abstract_form()}.

-spec add_definitions([erl_parse:abstract_form()], definition_map()) -> definition_map().
add_definitions(Forms, Defs) ->
    [Module] = [M || {attribute, _, module, M} <- Forms],
    lists:foldl(fun(Form, Acc) -> add_definition(Form, Module, Acc) end, Defs, Forms).

add_definition({attribute, _Loc, enum, {Name, _Type, _Vs}} = Enum, Module, Defs) ->
    maps:put({Module, Name}, Enum, Defs);
add_definition(_, _, Defs) ->
    Defs.
