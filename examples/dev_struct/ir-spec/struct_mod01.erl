-file("dev_struct/src/struct_mod01.erlt", 1).

-module(struct_mod01).

-export_type([foo/0,
              bar/1,
              default_with_default/0,
              unicodestruct/0]).

-export([foo/0,
         bar/0,
         baz/0,
         pattern/3,
         guard/1,
         field/1,
         update/1,
         index/1]).

-type bar(A) :: {'$#struct_mod01:bar', A, foo()}.

-type baz() :: {'$#struct_mod01:baz', bar(integer())}.

-type
     unicodestruct() :: {'$#struct_mod01:unicodestruct',
                         string()}.

-type in_pattern() :: {'$#struct_mod01:in_pattern'}.

-type
     default_with_default() :: {'$#struct_mod01:default_with_default',
                                baz()}.

-type foo() :: {'$#struct_mod01:foo'}.

foo() -> {'$#struct_mod01:foo'}.

bar() ->
    {'$#struct_mod01:bar', 1, {'$#struct_mod01:foo'}}.

baz() ->
    {{'$#struct_mod01:baz',
      {'$#struct_mod01:bar',
       erlang:trunc(1.5),
       {'$#struct_mod01:foo'}}},
     {'$#struct_mod01:default_with_default',
      {'$#struct_mod01:baz',
       {'$#struct_mod01:bar',
        erlang:trunc(1.5),
        {'$#struct_mod01:foo'}}}}}.

pattern({'$#struct_mod01:foo'},
        {'$#struct_mod01:bar', _, B},
        {'$#struct_mod01:in_pattern'}) ->
    B.

guard(Value)
    when Value =:= {'$#struct_mod01:bar', 1, 2} ->
    ok.

field(Value)
    when erlang:is_record(Value, '$#struct_mod01:bar', 3)
             orelse fail,
         erlang:element(2, Value) =:= 1 ->
    case Value of
        {'$#struct_mod01:bar', _, StructGenVar@0@b} ->
            StructGenVar@0@b;
        _ -> erlang:error({badstruct, '$#struct_mod01:bar'})
    end.

update(Value) ->
    case Value of
        {'$#struct_mod01:bar', _, StructGenVar@1@b} ->
            {'$#struct_mod01:bar', 2, StructGenVar@1@b};
        _ -> erlang:error({badstruct, '$#struct_mod01:bar'})
    end.

index(2) when 2 =:= 2 -> 3.



