-file("dev_enum/src/enum_mod12.erlt", 1).

-module(enum_mod12).

-unchecked([]).

-export([expr/0, pattern/3, guard/3]).

-spec expr() -> {enum_mod11:bar(), enum_mod11:baz()}.

expr() ->
    {{'$#enum_mod11:bar.x', 1, 2},
     {'$#enum_mod11:baz.x', 1, 2}}.

-spec pattern(enum_mod11:bar(), enum_mod11:baz(),
              number()) -> number().

pattern({'$#enum_mod11:bar.x', 1, B},
        {'$#enum_mod11:baz.x', 1, B}, B) ->
    B.

-spec guard(enum_mod11:bar(), enum_mod11:baz(),
            number()) -> atom().

guard(Value1, Value2, B)
    when Value1 =:= {'$#enum_mod11:bar.x', 1, B},
         Value2 =:= {'$#enum_mod11:baz.x', 1, B} ->
    ok.



