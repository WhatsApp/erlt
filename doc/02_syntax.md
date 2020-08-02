# 2. General Syntax

There are two layers/languages: the language of expressions and the language of
types. They are different in different modes.
See the [ST/FFI/DT modes](03_modes.md) document for an overview.

**General restrictions (all modes)**:

* Already bound variables are not allowed in patterns
* Relying on features of textual scoping is not allowed
    * We proceed only programs where lexical scoping of variables and textual
      scoping of variables would result in into the same runtime behaviour.

Currently, it is implemented via forbidding ambiguity in usage of variables.

## DT part

Syntactically, DT is a superset of Classic Erlang. It is classic Erlang extended
with constructs for *enums*, and "*dot access operator*" for maps.

**Enums**: See the [enums](04_enums.md) document.

**Dot access operator**: a syntactic sugar for invoking `erlang:map_get(K, M)`
when K is an atom literal. So, in DT the following two lines are equivalent:

```erlang
% Classic Erlang
erlang:map_get(property, M)
% DT
M.property
```

## ST part

### Expressions

A subset of DT language.

**Constructs not supported in ST part (at the syntax level).**

* `catch`-expressions
* Atom literals (except `true` and `false`)
* Functions with dynamic arities (`foo/Var` instead of `foo/2`).

**Map syntax is re-used for [polymorphic records](05_polymorphic_records.md)**

### Types

A subset of DT language.

**Classic Erlang constructs not supported in ST part (at the syntax level)**.

* Union types
* `when` clauses in specs
* "Incomplete types" (missing important information) - like
  `fun()`, `fun((...) -> Type)`
* Singleton types (like `1`, ``[]``)
* `maybe_improper_list`
* "Intersection types" for funs

See [ST mapping](06_st_mapping.md) about how types are interpreted.

## FFI part

* Expressions - the same logic as for DT expressions.
* Types - the same logic as for ST part.
