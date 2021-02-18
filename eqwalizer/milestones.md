# Eqwalizer milestones

## Jan-Feb 2021

Simple (naive) bidirectional type-checking. Main goal: to support full range of
Erlang surface syntax.

### Surface syntax

- [x] Catch
- [x] Try/catch
- [x] Try-of/catch
- [x] Receives
- [x] List comprehensions
- [x] Binary comprehensions
- [x] Maps
- [x] Records
- [x] `tuple()`, `list()`, `fun()`
- [ ] `catch X:Y:Z ->` syntax
- [ ] Anonymous functions
- [ ] Named inner functions (not priority)
- [ ] Recursive type definitions
- [ ] Opaques
- [ ] Simple polymorphism
- [ ] Simple inference for unspecced funs
- [ ] Numeric tower
- [ ] (Expr)(Arg1, Arg2) - checking that the head is proper fun
- [ ] Non-empty lists, maybe improper lists, etc

### More elaboration/propagation

- [ ] Propagate selector type in case-expressions
- [ ] Propagate try-body type in try-of expressions
- [ ] `when X = Y` -> `X` and `Y` are of the same type.

### Data-driven analysis

- [x] [Named inner functions](https://fb.workplace.com/groups/typederlang/permalink/268350134631822) 
- [ ] Private functions - recursive and non-recursive.
- [ ] Dependencies between modules and functions in general.

Anyway, we would implement simple inference for non-recursive functions first.
