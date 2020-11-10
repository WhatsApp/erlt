-file("dev_enum/src/enum_mod12.erlt", 1).

-module(enum_mod12).

-export([expr/0, pattern/3, guard/3]).

expr() ->
    {{'$#enum_mod11:bar.x', 1, 2},
     {'$#enum_mod11:baz.x', 1, 2}}.

pattern({'$#enum_mod11:bar.x', 1, B},
        {'$#enum_mod11:baz.x', 1, B}, B) ->
    B.

guard(Value1, Value2, B)
    when Value1 =:= {'$#enum_mod11:bar.x', 1, B},
         Value2 =:= {'$#enum_mod11:baz.x', 1, B} ->
    "ok".



