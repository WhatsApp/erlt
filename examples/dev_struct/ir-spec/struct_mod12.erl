-file("dev_struct/src/struct_mod12.erlt", 1).

-module(struct_mod12).

-export([expr/0,
         pattern/3,
         guard/3,
         field/1,
         update/1,
         index/1]).

expr() ->
    {{'$#struct_mod11:bar',
      1,
      {'$#struct_mod11:mixed', 1, 2}},
     {'$#struct_mod11:mixed', 1, 2}}.

pattern({'$#struct_mod11:bar', B, _},
        {'$#struct_mod11:mixed', B, B}, B) ->
    B.

guard(Value1, Value2, B)
    when Value1 =:= {'$#struct_mod11:bar', 1, B},
         Value2 =:= {'$#struct_mod11:mixed', 1, 2} ->
    "ok".

field(Value)
    when erlang:is_record(Value, '$#struct_mod11:bar', 3)
             orelse fail,
         erlang:element(2, Value) =:= 1 ->
    case Value of
        {'$#struct_mod11:bar', _, StructGenVar@0@positional} ->
            StructGenVar@0@positional;
        _ -> erlang:error({badstruct, '$#struct_mod11:bar'})
    end.

update(Value) ->
    case Value of
        {'$#struct_mod11:mixed',
         StructGenVar@1@positional,
         _} ->
            {'$#struct_mod11:mixed', StructGenVar@1@positional, 2};
        _ -> erlang:error({badstruct, '$#struct_mod11:mixed'})
    end.

index(2) when 2 =:= 2 -> 3.



