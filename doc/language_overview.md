# ErlT language overview

ErlT, an experimental Erlang dialect with first-class support for static typing.
For high-level description of ErlT and motivation behind it, see [ErlT
Vision](vision.md).

ErlT largely follows Erlang syntax and general model. The best way to describe
it is by highlighting its differences from Erlang.

This document describes various new and altered language features that
constitute ErlT.

New features include:
- first-class static typing, and interoperability between statically typed
  (checked) and dynamically typed (unchecked) code
- new first-class data types
    - enums, aka [discriminated union types](https://en.wikipedia.org/wiki/Algebraic_data_type)
    - structs, aka first-class nominal records
    - shapes, aka ad-hoc structured records
- improved variable scoping model: (almost) lexical scoping and more explicit
  rules for variable shadowing
- `import_type` - symmetrical construct to `export_type`

Departure from Erlang:
- records are not supported, they are replaced by structs
- restricted type and spec syntax and semantics, and it is only supported for
  checked code
- atoms must be quoted
- slightly different catch syntax

Some limitations of the current ErlT prototype:
- no stdlib, no framework, but rudimentary mechanisms for wrapping Erlang stdlib are provided
- no editor/IDE integration, nor other language tooling such as formatter
- no ErlT-specific shell (stock Erlang shell can be used instead)
- only basic rebar3 integration: no support for using one ErlT app as a
  dependency for another ErlT app
- when it comes to static typing:
  - no typing of concurrency primitives
  - rudimentary support for exceptions
  - no support for Erlang maps syntax, dynamic
  - simplistic typing model for numbers and strings

Things that didn't make it into this prototype, but considered for ErlT:
- ergonomics for structs and enums -- currently they require using explicit type
  name for all operations similar to Erlang records
- namespaces and namespace management
- language capabilities to aid the adoption of static typing
- tooling for migrating from Erlang to ErlT


## File extension

ErlT source files have `.erlt` extension.


## Static typing

By default, ErlT is checked (i.e. statically typed), but it also allows some
functions or modules to be unchecked (i.e. dynamically typed), both to aid
migration and to allow code to be written for which we don't yet have a good
typing model. ErlT requires adding specs to unchecked code in order for it to be
accessible and typeable from checked code.

Notably, checked ErlT is currently limited to only the sequential parts of
Erlang, with some features banned, for example:

- Erlang maps
- Dynamic applications (`M:F(A)`)

ErlT type system based on the
[Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system),
extended to support ErlT data types and pattern matching. Most notably, the type
system offers type inference, so specs are only required for exported functions.

In ErlT, you must provide specs for any functions exported from a module.
Example:

```erl
-spec function_name(Var::type) -> Var::type.
```

For non-exported functions, types are inferred, so specs are optional. You get
type-checking without any typing with hands.


## ErlT type and spec language

The forms allowed in ErlT types and specs are a subset of what is allowed in
Erlang, with a few additions for shapes, structs, and enums (see below
for their description).

Allowed in ErlT types and specs:
- `string()`
- `atom()`
- `list()`
- `number()`
- `integer()` and `float()`
- `binary()`
- shapes, structs, and enums
- tuples like `{integer(), string()}`
- ...

Not allowed in ErlT types and specs:
- unions (use enums instead)
- intersections
- guards
- ranges
- singleton types, such as `1`, `ok`
- types containing singleton types, such as `{error, string()}`
- maps
- `any()`


## Unchecked ErlT

To ease migration, some code can be unchecked. That means that the type checker
does not check the types:

```erl
[unchecked]
foo() -> ....
```

You can combine `[unchecked]` with type specs: then the type checker will
believe you, and allow you to export the functions. This can be convenient for
"wrapping" untyped code for use with typed code:

```erl
[unchecked]
-spec function_name(Var::type) -> Var::type.
function_name() -> ...
```

In unchecked ErlT, you can still use the new ErlT features (enums, structs and
shapes), but won't get the benefits of static type checking.

Unchecked code is useful in order to
- aid migration
- allow certain features to be expressed, even if they are hard or impossible to type
check at the moment, such as Erlang's concurrency.

### Unchecked Opaque modifier

`[unchecked, opaque]`

These two modifiers together in front of a type means that the type is an
opaque type in the statically typed world with a type definition which is
dynamic. For now the only unchecked types we allow are term().


## Interop between Checked and Unchecked ErlT

Unchecked code can call any ErlT code.

Checked code, however, needs types to check against, so to call unchecked code
that code needs to have a spec.


## Built-in types and modules

Built-ins is a rudimentary mechanism for wrapping Erlang stdlib's functionality,
and providing ErlT type specs for standard Erlang modules.

See [../builtins](../builtins) for details on how it works and how to use it.

For example, we define a new module `t_io` that contains one unchecked function
`format/2` — this function is very similar to `io:format/2`, but can be called from
checked ErlT code and accepts a tuple for arguments instead of a list - this
allows type-checking to work correctly without any special-casing. The function
uses a trick with a single-use parameter that allows the type of the argument
tuple to be “swallowed”.

```
-spec format(string(), _) -> atom().
[unchecked]
format(Fmt, Args) ->
    io:format(Fmt, tuple_to_list(Args)).
```

This would be called as, for example, `t_io:format("Hello, ~ts!", {"world"}).`

ErlT currently doesn't come with stdlib on its own. To get an idea what it could
looked like, see [../examples/elm_core/src](../examples/elm_core/src). It's a subset of
[Elm](https://elm-lang.org/)'s standard library ported to ErlT.


## Variable scoping

We want to reduce the scope for programming errors due to Erlang’s very
permissive approach to variable bindings, especially at larger scale when many
people are refactoring and modifying a large code base. We also want to keep our
options open for the future, where we may decide to allow more constructs to
shadow surrounding bindings. Therefore, we restrict the standard Erlang scoping
rules in the following way.

### Pinning operator (^)

We add a “pinning” operator*, ^, as in Elixir, for marking variables in
patterns as being evaluated in the surrounding scope rather than being new,
shadowing instances.

This change was suggested as an enhancement for the Erlang programming
language. See [EEP 55: Pinning operator ^ in
patterns](https://www.erlang.org/erlang-enhancement-proposals/eep-0055.html)
for examples and detailed description.

### No exported bindings from subexpressions

In current Erlang, not only match expressions like `X = ...` or `{X, Y} = ...`
create bindings which can be used in following expressions in the same body, but
also expressions like `f(X = g(...), ...)` will “export” the bound variables to
the following expressions. In particular, expressions like `case ... of {ok, X}
-> ok; error -> X = 0 end, bar(X)` are possible, because `X` will become bound
regardless of which branch is selected, and is thus guaranteed to have a value
at the point of `bar(X)`. If some but not all branches assign a value to a
variable, the Erlang compiler will signal an error if you to try to use the
variable outside the switch expression.

This kind of idiom can in some cases be quite useful and succinct, but can also
be a source of confusion. To keep ErlT variable scopes simple, we do not allow
variables to be exported from subexpressions, and only the match operator `=`
may be used to define variables to be used in a subsequent expression.

Erlang code which relies on variable export from case switches or other
subexpressions must be rewritten in order to be migrated to ErlT. We plan to
implement a migration tool capable of automating such rewrites in all or most
cases.

### Shadowing/rebinding not allowed, to begin with

Referring to a variable which has a previous definition in a subexpression in
the same function is a compile time error. Hence, variables in subexpressions
may not yet be used as if having purely local scope. 


## Atoms

In ErlT, we differentiate syntax for atoms in identifier position and in value
position.

### Identifiers

In identifier positions, ErlT supports both `'quoted'` and `unquoted` atoms, the
same as Erlang.

### Values

In value position, both in patterns and expressions, we only support `'quoted'`
atoms.

In checked code atoms have the `atom()` built-in type and behave similar to
integers or strings.

### Booleans

`true` and `false` were promoted to keywords and are still supported unquoted.


## Numbers

In both checked and unchecked code:
  - The numbers and operators allowed in the expression language are the same as
    for Erlang (no change): `3`, `3.1415`, `5 + 6`, `1.0 rem 3.0`, etc. will all
    be allowed and behave the same way as for Erlang
  - There is no compile-time protection from doing `1.0 div 3.0` - this will
    produce the same runtime error as it does in Erlang.

In the checked language:
  - `integer()` and `float()` are aliases for `number()`
  - things that are floats and integers at runtime will be of type `number()`
  - in the type language:
    - `char()`, `positive_ingeger()`, `negative_integer()` are aliased as
      `number()`
    - ranges are not supported

Note that the long-term solution will likely be different.

## Strings

TODO


## Structs

Structs conceptually meant to replace records, and Erlang record syntax is not
currently supported in ErlT.

Structs are similar to Erlang records, but they are module-scoped, and allow
positional fields.

See examples in
[../examples/erltodo/src/erltodo.erlt](../examples/erltodo/src/erltodo.erlt) and
[../tests/dev_struct/src](../tests/dev_struct/src).

Structs support five operations. Definition, construction, pattern matching,
update and indexing.

### Definition:

`-struct name(Type...) :: (TypeExpr1, field2 = DefaultValueExpr2 :: TypeExpr2, ...). `

Where the name and fields are atoms, `DefaultValueExpr` are constant
expressions. The type parameters and parentheses can be skipped if the struct
has no type parameters. Fields specified with just a type are positional, and
have to always be accessed positionally — positional fields cannot have default
values. Positional fields have to be defined before labeled fields.

### Construction:

`#module:name{FieldExpr1, field2=FieldExpr2,...}`
`#name{FieldExpr1, field2=FieldExpr2, ...}`

Where module fields and name are all atoms, `FieldExpr1` can be any expression
other than match expression (match expressions have to be wrapped in
parentheses), `FieldExpr2` can be any expression. Positional fields have to be
defined before labeled fields, labeled fields can be written in any order, and
FieldExprs will be evaluated in lexical order. If module is omitted the module
will be determined based on imports and if it is not imported the current module
will be used.

### Pattern Matching

`#module:name{PositionalPattern, field2=LabeledPattern,...}`
`#name{PositionalPattern, field2=LabeledPattern, ...}`

Where module fields and name are all atoms, `PositionalPattern` can be any
Erlang pattern other than match, match patterns have to be wrapped in
parentheses. `LabeledPattern` can be any Erlang pattern. All positional fields
have to be specified. Labeled fields can be written in any order and can be
omitted - in that case they behave as if they were matched against the `_`
wildcard pattern.

### Update

`Expr#module:name{field1=ValueExpr1,...}`
`Expr#name{field1=ValueExpr1,...}`

`Expr` is any Erlang expression, `module`, `name` and fields are all atoms,
`ValueExprs` are any Erlang expressions. If `Expr` does not evaluate to the
expected struct we have a runtime error. Fields can be written in any order.
Only labeled fields can be updated that way.

### Field access

`Expr#module:name.field`
`Expr#module:name._0`
`Expr#name.field`
`Expr#name._0`

Expr is any erlang expression, module, name and fields are all atoms. If Expr
does not evaluate to the expected struct we have a runtime error. Only labeled
fields can be accessed that way. Positional fields are indexed with `_0`,
through `_N` when `N` is the number of positional fields (`_N` instead of just
`N` is used to avoid lexer conflicts in `Expr#name.0#name2.0` — `0#` is parsed
as the base syntax for integers).

### Index access

`#module:name.field`
`#module:name._0`
`#name.field`
`#name._0`

This expands to an integer value that represents the tuple index of the field.
Primarily reason to have this is compatibility with the record expressions
`#name.field` used for functions like `lists:keyfind` or when storing records in
ETS tables.

### Representation

The default representation of structs will be as Erlang tuples ordered according
to definition. The tuples are tagged, by default, with `#$module:struct_name`


## Enums
   
Enums are also known as [discriminated union
types](https://en.wikipedia.org/wiki/Algebraic_data_type). They model tagged
union types, somewhat similar to Erlang tuples, but they are more explicit,
restrictive, and require explicit named definition.

ErlT enums are are similar to ADTs in OCaml, data in Haskell, and enums in Rust
and Swift (not surprisingly given the same).

See examples in
[../examples/elm_core/src/result.erlt](../examples/elm_core/src/result.erlt),
[../examples/elm_core/src/maybe.erlt](../examples/elm_core/src/maybe.erlt) and
[../tests/dev_enum/src/](../tests/dev_enum/src/).

Enums support three operations, definition, construction and pattern matching.

### Definition

`-enum name(Type,...) :: (constructor{field1=DefaultValue1 :: Type,...}, ... ).`

The type parameters and parentheses can be skipped if the struct has no type
parameters. The syntax for field definitions is identical to structs defined
above.

### Construction:

`module:name.constructor{field1=FieldExpr1,...}`
`name.constructor{field1=FieldExpr1,...}`

Where module, fields, name and constructor are all atoms, FieldExprs can be any
expression. If module is omitted the current module will be implied. The syntax
for field definition is identical to structs defined above.

### Pattern Matching

`module:name.constructor{field1=Pattern1,...}`
`name.constructor{field1=Pattern1,...}`

Where module fields and name are all atoms, Pattern can be any Erlang pattern.
If module is omitted the current module will be implied. The syntax for field
definitions is identical to structs defined above.

### Representation

Enums will be represented exactly the same as structs, with the exception of
tags that take the form of `#$module:enum_name.variant_name`.


## Shapes

This section describes “shapes” (aka anonymous or ad-hoc structs). A key aspect
of the current design is that shapes do not replace the maps syntax which will
exist side by side.

Shapes doen’t require definition. The main use case for shapes is a
generalization of a tuple, which allows named fields vs only positional ones in
a tuple. Intuitively, anonymous structs can be seen as Erlang map with atom keys
with invariably more restricted semantics in statically typed model.

For shapes, we support four operations. Construction, pattern matching, update
and indexing. In the examples `‘...’ `is never part of the concrete syntax it is
only used to express more expressions of the same form.

See examples in
[../tests/dev_struct/src/shape_mod1.erlt](../tests/dev_struct/src/shape_mod1.erlt).


### Construction:

`#(field1=FieldExpr1, ...)`

Where module fields and name are all atoms, FieldExprs can be any expression. If
module is omitted the current module will be implied which goes for all of these
constructs.

### Pattern Matching

`#(field1=Pattern1,...)`

Where module fields and name are all atoms, Pattern can be any Erlang pattern.
This pattern matches in the open style e.g. the struct does not have to have
exactly these fields, but at least these fields.

### Update

`Expr#(field1=ValueExpr1,...)`

`Expr` is any Erlang expression, `module`, `name` and fields are all atoms,
`ValueExprs` are any Erlang expressions. If `Expr` does not evaluate to a struct
we have a runtime error.

### Indexing

`Expr#(field)`

`Expr` is any Erlang expression, `module`, `name` and fields are all atoms. If
`Expr` does not evaluate to the expected struct we have a runtime error.

### Type syntax

`#(field::Type,...)`

In the type language represents a shape with exactly these fields.

`#(field::Type,..., Var)`

Represents an shape with at least these fields, and a Type variable representing
the extension.

### Representation

A shape is represented as an Erlang map with atom keys.


## Maps in ErlT

The Erlang map syntax is only allowed in functions which are marked as unchecked
in ErlT, where it has exactly the same semantics as maps in Erlang. If the map
syntax is used anywhere else including in type definitions a compiler error is
raised.

This is valid ErlT:

```
[unchecked]
f(#{a:=B}) -> B#{a => 1}.
```

These are all forbidden:

```
f(#{a:=B}) -> B#{a => 1}.

-type integer_map :: #{integer() => integer()}.

-struct map_struct :: (mapping = #{})
```


## Exceptions

Exception support is currently very limited in checked code.

See examples in
[../tests/dev/src/catch_and_throw.erlt](../tests/dev/src/catch_and_throw.erlt)
and [../tests/dev/src/try_catch.erlt](../tests/dev/src/try_catch.erlt)

### Catch

The syntax is the same as in Erlang, except for `catch` clauses where segments
are separated with `,` instead of `:`

```
try
    Exprs
catch
    Kind, Reason, Stacktrace -> {Kind, Reason, Stacktrace}
end.
```

We can't use the current Erlang syntax and allow enum syntax without any
prefixes `foo:bar{}` - if you see `error:foo{}` in catch you don't know if it's
qualified constructor `error:foo` and implicit `throw`, or explicit `error`
with an unqualified `foo` constructor.


## `import_type`

ErlT introduces a new attribute `-import_type(module, [type1, type2, ...])`
which allows using types from remote modules via short names (as if they were
defined in the current module). If the imported type is an enum, the enum
constructors can be used as the enum was defined in the current module.

Example from [../examples/elm_core/src/](../examples/elm_core/src/):

### Without `import_type`

`maybe.erl`:

<pre>
-module(maybe).

-export_type([maybe/1]).

-export([with_default/2]).

-enum maybe(A) :: (just{A}, nothing{}).

-spec with_default(A, maybe(A)) -> A.
with_default(_Default, maybe.just{Value}) -> Value;
with_default(Default, maybe.nothing{}) -> Default.

...
</pre>

`result.erl`:

<pre>
-module(result).

-export_type([result/2]).
-export([to_maybe/1, from_maybe/2]).

-enum result(Error, Value) :: (ok{Value}, err{Error}).

-spec to_maybe(result(_, A)) -> <b>maybe:maybe(A)</b>.
to_maybe(result.ok{V}) -> <b>maybe.maybe.just{V}</b>;
to_maybe(result.err{_}) -> <b>maybe.maybe.nothing{}</b>.

-spec from_maybe(X, <b>maybe:maybe(A))</b> -> result(X, A).
from_maybe(_Err, <b>maybe.maybe.just{V}</b>) -> result.ok{V};
from_maybe(Err, <b>maybe.maybe.nothing{}</b>) -> result.err{Err}.
</pre>

### With `import_type`

`result.erl`:

<pre>
-module(result).

<b>-import_type(maybe, [maybe/1]).</b>

-export_type([result/2]).
-export([to_maybe/1, from_maybe/2]).

-enum result(Error, Value) :: (ok{Value}, err{Error}).

-spec to_maybe(result(_, A)) -> <b>maybe(A)</b>.
to_maybe(result.ok{V}) -> <b>maybe.just{V}</b>;
to_maybe(result.err{_}) -> <b>maybe.nothing{}</b>.

-spec from_maybe(X, <b>maybe(A)</b>) -> result(X, A).
from_maybe(_Err, <b>maybe.just{V}</b>) -> result.ok{V};
from_maybe(Err, <b>maybe.nothing{}</b>) -> result.err{Err}.
</pre>


## Known problems and limitations

### Cannot use types from built-in modules

Declare a type `-type foo() :: ......` in a built-in module in erlt/builtins.

Try to use the type from an erlt file anywhere under tests or examples: either qualified or with import_type

Actual behavior: fails dependency analysis

Expected behavior: can use the type from a built-in module

### unOp and binOp in patterns are not supported in checked code

For example:
```
b27(<<Size1, Size2, Bin:(Size1 + Size2)/binary>>) ->
    true.
```

### Unbound but repeated type vars should be reported as errors

No errors are reported there, but in all the cases `A` is unbound:
```
-struct bad_struct :: (x :: A, y :: A).
-enum either :: (left{A}, right{A}).
-type bad_tuple() :: {A, A}.
```
