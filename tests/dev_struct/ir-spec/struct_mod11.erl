-file("dev_struct/src/struct_mod11.erlt", 1).

-module(struct_mod11).

-eqwalizer_unchecked([{index, 1}]).

-export_type([foo/0,
              bar/1,
              mixed/0,
              baz/0,
              in_pattern/0]).

-export([foo/0,
         bar/0,
         baz/0,
         pattern/3,
         guard/1,
         field/1,
         update/1,
         index/1]).

-type bar(A) :: {'$#struct_mod11:bar', A, mixed()}.

-type mixed() :: {'$#struct_mod11:mixed',
                  integer(),
                  integer()}.

-type baz() :: {'$#struct_mod11:baz', bar(integer())}.

-type in_pattern() :: {'$#struct_mod11:in_pattern'}.

-type foo() :: {'$#struct_mod11:foo'}.

-spec foo() -> foo().

foo() -> {'$#struct_mod11:foo'}.

-spec bar() -> bar(integer()).

bar() ->
    {'$#struct_mod11:bar',
     1,
     {'$#struct_mod11:mixed', 1, 2}}.

-spec baz() -> baz().

baz() -> {'$#struct_mod11:baz', bar()}.

-spec pattern(foo(), bar(B), in_pattern()) -> B.

pattern({'$#struct_mod11:foo'},
        {'$#struct_mod11:bar', B, _},
        {'$#struct_mod11:in_pattern'}) ->
    B.

-spec guard(bar(integer())) -> string().

guard(Value)
    when Value =:=
             {'$#struct_mod11:bar',
              1,
              {'$#struct_mod11:mixed', 1, 2}} ->
    "ok".

-spec field(bar(integer())) -> {mixed(), integer()}.

field(Value)
    when erlang:is_record(Value, '$#struct_mod11:bar', 3)
             orelse fail,
         erlang:element(2, Value) =:= 1 ->
    {case Value of
         {'$#struct_mod11:bar', _, StructGenVar@0@positional} ->
             StructGenVar@0@positional;
         _ -> erlang:error({badstruct, '$#struct_mod11:bar'})
     end,
     case case Value of
              {'$#struct_mod11:bar', _, StructGenVar@2@positional} ->
                  StructGenVar@2@positional;
              _ -> erlang:error({badstruct, '$#struct_mod11:bar'})
          end
         of
         {'$#struct_mod11:mixed', _, StructGenVar@1@a} ->
             StructGenVar@1@a;
         _ -> erlang:error({badstruct, '$#struct_mod11:mixed'})
     end}.

-spec update(mixed()) -> mixed().

update(Value) ->
    case Value of
        {'$#struct_mod11:mixed',
         StructGenVar@3@positional,
         _} ->
            {'$#struct_mod11:mixed', StructGenVar@3@positional, 2};
        _ -> erlang:error({badstruct, '$#struct_mod11:mixed'})
    end.

index(2) when 2 =:= 2 -> 3.



