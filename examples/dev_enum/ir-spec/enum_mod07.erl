-file("dev_enum/src/enum_mod07.erlt", 1).

-module(enum_mod07).

-export([expr/0,
         pattern/3,
         guard/3,
         remote_with_defaults/0]).

-export_type([with_imported_default/0]).

-import_type({enum_mod04, [{foo, 0}, {bar, 0}]}).

-type
     with_imported_default() :: {'$#enum_mod07:with_imported_default.x',
                                 enum_mod04:foo()}.

expr() ->
    {{'$#enum_mod04:foo.x'}, {'$#enum_mod04:bar.x', 1, 2}}.

pattern({'$#enum_mod04:foo.x'},
        {'$#enum_mod04:bar.x', 1, B}, B) ->
    B.

-spec guard(enum_mod04:foo(), enum_mod04:bar(),
            number()) -> atom().

guard(Value1, Value2, B)
    when Value1 =:= {'$#enum_mod04:foo.x'},
         Value2 =:= {'$#enum_mod04:bar.x', 1, B} ->
    ok.

remote_with_defaults() ->
    {{'$#enum_mod03:default_with_default.x',
      {'$#enum_mod03:baz.x',
       {'$#enum_mod03:bar.x', 1, {'$#enum_mod03:foo.x'}}}},
     {'$#enum_mod07:with_imported_default.x',
      {'$#enum_mod04:foo.x'}}}.



