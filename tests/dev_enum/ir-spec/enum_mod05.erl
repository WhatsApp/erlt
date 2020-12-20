-file("dev_enum/src/enum_mod05.erlt", 1).

-module(enum_mod05).

-unchecked([]).

-export([expr/0, pattern/3, guard/3]).

-spec expr() -> {enum_mod04:foo(),
                 enum_mod04:bar(),
                 enum_mod04:baz()}.

expr() ->
    {{'$#enum_mod04:foo.x'},
     {'$#enum_mod04:bar.x', 1, 2},
     {'$#enum_mod04:baz.x', #{id => 1}}}.

-spec pattern(enum_mod04:foo(), enum_mod04:bar(),
              number()) -> number().

pattern({'$#enum_mod04:foo.x'},
        {'$#enum_mod04:bar.x', 1, B}, B) ->
    B.

-spec guard(enum_mod04:foo(), enum_mod04:bar(),
            number()) -> atom().

guard(Value1, Value2, B)
    when Value1 =:= {'$#enum_mod04:foo.x'},
         Value2 =:= {'$#enum_mod04:bar.x', 1, B} ->
    ok.



