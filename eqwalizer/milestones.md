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
- [ ] Anonymous functions
- [ ] Named inner functions (not priority)
- [ ] Recursive type definitions
- [ ] Opaques
- [ ] Simple polymorphism
- [ ] Simple inference for unspecced funs
- [ ] Numeric tower
- [ ] (Expr)(Arg1, Arg2) - checking that the head is proper fun

### Low-priority, edgy, rare cases

- [ ] Non-empty lists, maybe improper lists, etc
- [ ] `fun((...) -> Type)` - "any arity type"
- [x] `is_function(F, Arity)` type predicate
- [ ] `bitstring()` vs `binary()` distinction - do we really distinguish these
       cases semantically in WA code base?

### More elaboration/propagation

- [x] Propagate selector type in case-expressions
  - [x] `catch X:Y:Z ->` syntax (depends of adding more propagation logic)
- [ ] Propagate try-body type in try-of expressions
- [ ] `when X = Y` -> `X` and `Y` are of the same type.

### Data-driven analysis

- [x] [Named inner functions](https://fb.workplace.com/groups/typederlang/permalink/268350134631822) 
- [ ] Private functions - recursive and non-recursive.
- [ ] Dependencies between modules and functions in general.

Anyway, we would implement simple inference for non-recursive functions first.
