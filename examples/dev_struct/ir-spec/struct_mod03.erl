-file("dev_struct/src/struct_mod03.erlt", 1).

-module(struct_mod03).

-export([expr/0,
         pattern/3,
         guard/3,
         field/1,
         update/1,
         index/1]).

expr() ->
    {{'$#struct_mod02:foo'}, {'$#struct_mod02:bar', 1, 2}}.

pattern({'$#struct_mod02:foo'},
        {'$#struct_mod02:bar', 1, B}, B) ->
    B.

guard(Value1, Value2, B)
    when Value1 =:= {'$#struct_mod02:foo'},
         Value2 =:= {'$#struct_mod02:bar', 1, B} ->
    "ok".

field(Value)
    when erlang:is_record(Value, '$#struct_mod02:bar', 3)
             orelse fail,
         erlang:element(2, Value) =:= 1 ->
    case Value of
        {'$#struct_mod02:bar', _, StructGenVar@0@b} ->
            StructGenVar@0@b;
        _ -> erlang:error({badstruct, '$#struct_mod02:bar'})
    end.

update(Value) ->
    case Value of
        {'$#struct_mod02:bar', _, StructGenVar@1@b} ->
            {'$#struct_mod02:bar', 2, StructGenVar@1@b};
        _ -> erlang:error({badstruct, '$#struct_mod02:bar'})
    end.

index(2) when 2 =:= 2 -> 3.



