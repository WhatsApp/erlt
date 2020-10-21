-file("dev_struct/src/struct_mod11.erlt", 1).

-module(struct_mod11).

-export_type([foo/0, bar/1]).

-export([foo/0, bar/0, baz/0, pattern/3, guard/1]).

-type bar(A) :: {'$#struct_mod11:bar', A, foo()}.

-type mixed() :: {'$#struct_mod11:mixed',
                  integer(),
                  integer()}.

-type baz() :: {'$#struct_mod11:baz', bar(integer())}.

-type in_pattern() :: {'$#struct_mod11:in_pattern'}.

-type foo() :: {'$#struct_mod11:foo'}.

foo() -> {'$#struct_mod11:foo'}.

bar() ->
    {'$#struct_mod11:bar', 1, {'$#struct_mod11:foo'}}.

baz() -> {'$#struct_mod11:baz', bar()}.

pattern({'$#struct_mod11:foo'},
        {'$#struct_mod11:bar', B, _},
        {'$#struct_mod11:in_pattern'}) ->
    B.

guard(Value)
    when Value =:= {'$#struct_mod11:bar', 1, 2} ->
    ok.

update(Value) ->
    case Value of
        {'$#struct_mod11:mixed',
         StructGenVar@0@positional,
         _} ->
            {'$#struct_mod11:mixed', StructGenVar@0@positional, 2};
        _ -> erlang:error({badstruct, '$#struct_mod11:mixed'})
    end.



