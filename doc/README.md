# ErlT Reference

This directory contains detailed descriptions of the various new and
altered language forms that constitute ErlT.

## File extension

The type checker is run on files with the `.erlt` extension.

## Type Checking

By default, ErlT code is checked, which is to say it is _statically typed_ with
a type system based on the
[the Hindley-Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system),
extended to support ErlT data types and pattern matching. Most notably, the
type system offers type inference, so specs are only required for exported functions.

Beyond ruling out whole classes of bugs and enforcing a standardised
structure to code, checked ErlT also opens the door to deeper IDE integration and other tooling.

In ErlT, you must provide specs for any functions exported from a module. Example:

```erl
-spec function_name(Var::type) -> Var::type.
```

For non-exported functions, types are inferred, so specs are optional. You get type-checking without any typing with hands.

## ErlT Specs

The forms allowed in ErlT specs are a subset of what is allowed in classic Erlang, with a few additions for [shapes](./shapes.md), [structs, and enums](./structs_and_enums.md).

Allowed in ErlT specs:
- string()
- atom()
- list()
- number()
- integer() and float(): these are currently aliases for number(), but can become more strict in future
- [shapes](./shapes.md), [structs, and enums](./structs_and_enums.md)
- ...

Not allowed in ErlT specs:
- unions (use [enums](./structs_and_enums.md) instead)
- intersections
- guards
- ranges

## Unchecked ErlT

To ease migration, some code can be unchecked. That means that the type checker does not check the types:

```erl
[unchecked]
foo() -> ....
```

You can combine `[unchecked]` with type specs: then the type checker will believe you, and allow you to export the functions. This can be convenient for "wrapping" untyped code for use with typed code:

```erl
[unchecked]
-spec function_name(Var::type) -> Var::type.
function_name() -> ...
```

In unchecked ErlT, you can still use the new ErlT features (enums, structs and shapes), but won't get the benefits of static type checking.

Unchecked code is useful in order to
- aid migration
- allow certain features to be expressed, even if they are hard or impossible to type
check at the moment, such as Erlang's concurrency.

## Interop between Checked and Unchecked ErlT

Unchecked code can call any ErlT code.

Checked code, however, needs types to check against, so to call unchecked code
that code needs to have a spec.
