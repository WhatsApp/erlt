-file("dev_enum/src/enum_mod05.erlt", 1).

-module(enum_mod05).

-export([expr/0, pattern/3, guard/3]).

expr() ->
    {{'$#enum_mod04:foo.x'},
     {'$#enum_mod04:bar.x', 1, 2},
     {'$#enum_mod04:baz.x', #{id => 1}}}.

pattern({'$#enum_mod04:foo.x'},
        {'$#enum_mod04:bar.x', 1, B}, B) ->
    B.

guard(Value1, Value2, B)
    when Value1 =:= {'$#enum_mod04:foo.x'},
         Value2 =:= {'$#enum_mod04:bar.x', 1, B} ->
    "ok".



