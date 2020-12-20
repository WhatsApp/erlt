-file("dev_enum/src/enum_mod03.erlt", 1).

-module(enum_mod03).

-unchecked([{bar, 0},
            {baz, 0},
            {foo, 0},
            {pattern, 3}]).

-export_type([foo/0,
              bar/1,
              default_with_default/0,
              opaque_baz/0]).

-export([foo/0, bar/0, baz/0, pattern/3, guard/1]).

-type bar(A) :: {'$#enum_mod03:bar.x', A, foo()}.

-opaque opaque_baz() :: baz().

-type baz() :: {'$#enum_mod03:baz.x', bar(integer())}.

-type in_pattern() :: {'$#enum_mod03:in_pattern.x'}.

-type
     default_with_default() :: {'$#enum_mod03:default_with_default.x',
                                opaque_baz()}.

-type foo() :: {'$#enum_mod03:foo.x'}.

foo() -> {'$#enum_mod03:foo.x'}.

bar() ->
    {'$#enum_mod03:bar.x', 1, {'$#enum_mod03:foo.x'}}.

baz() ->
    {{'$#enum_mod03:baz.x',
      {'$#enum_mod03:bar.x', 1, {'$#enum_mod03:foo.x'}}},
     {'$#enum_mod03:default_with_default.x',
      {'$#enum_mod03:baz.x',
       {'$#enum_mod03:bar.x', 1, {'$#enum_mod03:foo.x'}}}}}.

pattern({'$#enum_mod03:foo.x'},
        {'$#enum_mod03:bar.x', _, B},
        {'$#enum_mod03:in_pattern.x'}) ->
    B.

-spec guard(bar(number())) -> atom().

guard(Value)
    when Value =:=
             {'$#enum_mod03:bar.x', 1, {'$#enum_mod03:foo.x'}} ->
    ok.



