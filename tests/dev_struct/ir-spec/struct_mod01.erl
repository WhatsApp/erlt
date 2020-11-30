-file("dev_struct/src/struct_mod01.erlt", 1).

-module(struct_mod01).

-export_type([foo/0,
              bar/1,
              default_with_default/0,
              unicodestruct/0,
              opaque_baz/0,
              in_pattern/0]).

-export([foo/0,
         bar/0,
         baz/0,
         pattern/3,
         guard/1,
         field/1,
         update/1,
         index/1]).

-type bar(A) :: {'$#struct_mod01:bar', A, foo()}.

-opaque opaque_baz() :: baz().

-type baz() :: {'$#struct_mod01:baz', bar(integer())}.

-type
     unicodestruct() :: {'$#struct_mod01:unicodestruct',
                         string()}.

-type in_pattern() :: {'$#struct_mod01:in_pattern'}.

-type
     default_with_default() :: {'$#struct_mod01:default_with_default',
                                opaque_baz()}.

-type foo() :: {'$#struct_mod01:foo'}.

-spec foo() -> foo().

foo() -> {'$#struct_mod01:foo'}.

-spec bar() -> bar(integer()).

bar() ->
    {'$#struct_mod01:bar', 1, {'$#struct_mod01:foo'}}.

-spec baz() -> {opaque_baz(), default_with_default()}.

baz() ->
    {{'$#struct_mod01:baz',
      {'$#struct_mod01:bar', 1.5, {'$#struct_mod01:foo'}}},
     {'$#struct_mod01:default_with_default',
      {'$#struct_mod01:baz',
       {'$#struct_mod01:bar', 1.5, {'$#struct_mod01:foo'}}}}}.

-spec pattern(foo(), bar(_A), in_pattern()) -> foo().

pattern({'$#struct_mod01:foo'},
        {'$#struct_mod01:bar', _, B},
        {'$#struct_mod01:in_pattern'}) ->
    B.

-spec guard(bar(integer())) -> string().

guard(Value)
    when Value =:=
             {'$#struct_mod01:bar', 1, {'$#struct_mod01:foo'}} ->
    "ok".

-spec field(bar(integer())) -> foo().

field(Value)
    when erlang:is_record(Value, '$#struct_mod01:bar', 3)
             orelse fail,
         erlang:element(2, Value) =:= 1 ->
    case Value of
        {'$#struct_mod01:bar', _, StructGenVar@0@b} ->
            StructGenVar@0@b;
        _ -> erlang:error({badstruct, '$#struct_mod01:bar'})
    end.

-spec update(bar(integer())) -> bar(integer()).

update(Value) ->
    case Value of
        {'$#struct_mod01:bar', _, StructGenVar@1@b} ->
            {'$#struct_mod01:bar', 2, StructGenVar@1@b};
        _ -> erlang:error({badstruct, '$#struct_mod01:bar'})
    end.

index(2) when 2 =:= 2 -> 3.



