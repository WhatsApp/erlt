# Generic Executor in Idris (via GADTS)

This is a sequel to [`02_gen_exec_gadt`](../02_gen_exec_gadt).

How to run the end-to-end example:

```
$ idris --build gen_exec.ipkg
$ ./counter_test.exe
True
```

The main point of this exercise is to "hide the truth" of how much stuff is
implicitly propagated under the hood in order to NOT pass types at expression
level.

## Explicit implicitness

The mechanics is 
[implicit arguments](http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#implicit-arguments).

```idris
handleCall : {a : Type} -> (callT a) -> stateT -> (a, stateT)
```

The notation `{a : Type}` instructs the system (Idris): you should infer 
the type `a` from the context.

Note that we propagate these "instructions" down the line through 
the chain of abstractions:

```idris
execCall : {a : Type} -> (callT a) -> Eff a [STATE stateT]
sCall : {a : Type} -> (callT a) -> Eff a [STATE stateT]
``` 

But finally, - when working with concrete types and values we can omit the type
of the reply and Idris is able to infer (or more precisely: to elaborate) it:

```idris
equalImpl i = execCall exec (Equal i)
```

So, - at this particular moment proper `a` is inferred (from `Call a`).
