-file("dev_enum/src/enum_mod04.erlt", 1).

-module(enum_mod04).

-export_type([foo/0, bar/0, baz/0]).

-export([expr/0, pattern/3, guard/3]).

-type foo() :: {'$#enum_mod04:foo.x'}.

-type bar() :: {'$#enum_mod04:bar.x',
                integer(),
                integer()}.

-type baz() :: {'$#enum_mod04:baz.x',
                #{id := integer()}}.

expr() ->
    {{'$#enum_mod04:foo.x'},
     {'$#enum_mod04:bar.x', 1, 2},
     {'$#enum_mod04:baz.x', #{id => 1}}}.

pattern({'$#enum_mod04:foo.x'},
        {'$#enum_mod04:bar.x', 1, B}, B) ->
    B.

-spec guard(foo(), bar(), number()) -> atom().

guard(Value1, Value2, B)
    when Value1 =:= {'$#enum_mod04:foo.x'},
         Value2 =:= {'$#enum_mod04:bar.x', 1, B} ->
    ok.



