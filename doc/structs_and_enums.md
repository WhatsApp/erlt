# Minimal ErlT struct and enum design

The goal of this document is describe the design of structs and enums for the initial release of the ErlT prototype. A key part of this proposal is that it replaces records, meaning that the record syntax is not supported in ErlT. We can change {} to () but even with that we should not support both in ErlT.

# Syntax

## Structs

For structs we support five operations. Definition, construction, pattern matching, update and indexing.

### Definition:

`-struct name(Type...) :: (TypeExpr1, field2 = DefaultValueExpr2 :: TypeExpr2, ...). `

Where the name and fields are atoms,  DefaultValueExpr are constant expressions. The type parameters and parentheses can be skipped if the struct has no type parameters. Fields specified with just a type are positional, and have to always be accessed positionally — positional fields cannot have default values. Positional fields have to be defined before labelled fields.

### Construction:

`#module:name{FieldExpr1, field2=FieldExpr2,...}`
`#name{FieldExpr1, field2=FieldExpr2, ...}`

Where module fields and name are all atoms, `FieldExpr1` can be any expression other than match expression (match expressions have to be wrapped in parentheses), `FieldExpr2` can be any expression. Positional fields have to be defined before labelled fields, labelled fields can be written in any order, and FieldExprs will be evaluated in lexical order. If module is omitted the module will be determined based on imports and if it is not imported the current module will be used.

See more examples and discussion here: https://forum.erl2.org/t/design-of-structs-and-enums-for-erlt/69/41

### Pattern Matching

`#module:name{PositionalPattern, field2=LabelledPattern,...}`
`#name{PositionalPattern, field2=LabelledPattern, ...}`

Where module fields and name are all atoms, `PositionalPattern` can be any Erlang pattern other than match, match patterns have to be wrapped in parentheses. `LabelledPattern` can be any Erlang pattern. All positional fields have to be specified. Labelled fields can be written in any order and can be omitted - in that case they behave as if they were matched against the `_` wildcard pattern.

### Update

`Expr#module:name{field1=ValueExpr1,...}`
`Expr#name{field1=ValueExpr1,...}`


Expr is any erlang expression, module, name and fields are all atoms, ValueExprs are any erlang expressions. If Expr does not evaluate to the expected struct we have a runtime error. Fields can be written in any order. Only labelled fields can be updated that way.

### Field access

`Expr#module:name.field`
`Expr#module:name._0`
`Expr#name.field`
`Expr#name._0`

Expr is any erlang expression, module, name and fields are all atoms. If Expr does not evaluate to the expected struct we have a runtime error. Only labelled fields can be accessed that way.
Positional fields are indexed with `_0`, through `_N` when `N` is the number of positional fields (`_N` instead of just `N` is used to avoid lexer conflicts in `Expr#name.0#name2.0` — `0#` is parsed as the base syntax for integers).

### Index access

`#module:name.field`
`#module:name._0`
`#name.field`
`#name._0`

This expands to an integer value that represents the tuple index of the field.
Primarily reason to have this is compatibility with the record expressions `#name.field` used for functions like `lists:keyfind` or when storing records in ETS tables.

## Enums

Enums support three operations, definition, construction and pattern matching.

### Definition

`-enum name(Type,...) :: (constructor{field1=DefaultValue1 :: Type,...}, ... ).`

The type parameters and parentheses can be skipped if the struct has no type parameters. The syntax for field definitions is identical to structs defined above.

### Construction:

`module:name.constructor{field1=FieldExpr1,...}`
`name.constructor{field1=FieldExpr1,...}`

Where module, fields, name and constructor are all atoms, FieldExprs can be any expression. If module is omitted the current module will be implied. The syntax for field definition is identical to structs defined above.

### Pattern Matching

`module:name.constructor{field1=Pattern1,...}`
`name.constructor{field1=Pattern1,...}`

Where module fields and name are all atoms, Pattern can be any Erlang pattern. If module is omitted the current module will be implied. The syntax for field definitions is identical to structs defined above.

## Anon structs

[Minimal ErlT Shapes spec](https://fb.quip.com/8hzlAnda5Mjd)


# Representation

### Structs

The default representation of structs will be as Erlang tuples ordered according to definition. The tuples are tagged, by default, with `#$module:struct_name`

### Enums

Enums will be represented exactly the same as structs, with the exception of tags that take the form of `#$module:enum_name.variant_name`.

