-file("dev_struct/src/struct_mod12.erlt", 1).

-module(struct_mod12).

-unchecked([{index, 1}]).

-export([expr/0,
         pattern/3,
         guard/3,
         field/1,
         update/1,
         index/1]).

-spec expr() -> {struct_mod11:bar(number()),
                 struct_mod11:mixed()}.

expr() ->
    {{'$#struct_mod11:bar',
      1,
      {'$#struct_mod11:mixed', 1, 2}},
     {'$#struct_mod11:mixed', 1, 2}}.

-spec pattern(struct_mod11:bar(number()),
              struct_mod11:mixed(), number()) -> number().

pattern({'$#struct_mod11:bar', B, _},
        {'$#struct_mod11:mixed', B, B}, B) ->
    B.

-spec guard(struct_mod11:bar(number()),
            struct_mod11:mixed(), struct_mod11:mixed()) -> atom().

guard(Value1, Value2, B)
    when Value1 =:= {'$#struct_mod11:bar', 1, B},
         Value2 =:= {'$#struct_mod11:mixed', 1, 2} ->
    ok.

-spec
     field(struct_mod11:bar(number())) -> struct_mod11:mixed().

field(Value)
    when erlang:is_record(Value, '$#struct_mod11:bar', 3)
             orelse fail,
         erlang:element(2, Value) =:= 1 ->
    case Value of
        {'$#struct_mod11:bar', _, StructGenVar@0@positional} ->
            StructGenVar@0@positional;
        _ -> erlang:error({badstruct, '$#struct_mod11:bar'})
    end.

-spec
     update(struct_mod11:mixed()) -> struct_mod11:mixed().

update(Value) ->
    case Value of
        {'$#struct_mod11:mixed',
         StructGenVar@1@positional,
         _} ->
            {'$#struct_mod11:mixed', StructGenVar@1@positional, 2};
        _ -> erlang:error({badstruct, '$#struct_mod11:mixed'})
    end.

index(2) when 2 =:= 2 -> 3.



