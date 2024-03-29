-file("dev_enum/src/enum_mod11.erlt", 1).

-module(enum_mod11).

-eqwalizer_unchecked([]).

-export([expr/0, pattern/3, guard/3]).

-export_type([bar/0, baz/0]).

-type bar() :: {'$#enum_mod11:bar.x',
                integer(),
                integer()}.

-type baz() :: {'$#enum_mod11:baz.x',
                integer(),
                integer()}.

-spec expr() -> {bar(), baz()}.

expr() ->
    {{'$#enum_mod11:bar.x', 1, 2},
     {'$#enum_mod11:baz.x', 1, 2}}.

-spec pattern(bar(), baz(), number()) -> number().

pattern({'$#enum_mod11:bar.x', 1, B},
        {'$#enum_mod11:baz.x', 1, B}, B) ->
    B.

-spec guard(bar(), baz(), number()) -> atom().

guard(Value1, Value2, B)
    when Value1 =:= {'$#enum_mod11:bar.x', 1, B},
         Value2 =:= {'$#enum_mod11:baz.x', 1, B} ->
    ok.



