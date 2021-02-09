# Experimental Checker for Core Erlang

in a shell, in this directory:

```sh
erlc +debug_info core_analyzer.erl && erl -sname core_analyzer@localhost -pa
```

in another shell in this directory:

```sh
sbt test
```

## Supported Features

### Surface syntax (TODO)
- [x] Catch
- [x] Try
- [x] Receives
- [x] `try ... catch X:Y:Z ->` syntax
- [ ] List comprehensions
- [ ] Binary comprehensions
- [ ] List comprehensions
- [ ] Binary comprehensions
- [ ] Maps
- [ ] Records
- [ ] Anonymous functions
- [ ] Named inner functions (not priority)
- [ ] Recursive type definitions
- [ ] Opaques
- [ ] Simple polymorphism
- [ ] Simple inference for unspecced funs
- [ ] Numeric tower
- [ ] Non-empty lists, maybe improper lists, etc
- [ ] `tuple()`, `list()`
- [ ] (Expr)(Arg1, Arg2) - checking that the head is proper fun
### More elaboration/propagation
- [ ] Propagate selector type in case-expressions
- [ ] Propagate try-body type in try-of expressions
### Data-driven analysis
- [ ] Named inner functions
- [ ] Private functions - recursive and non-recursive.
- [ ] Dependencies between modules and functions in general.
