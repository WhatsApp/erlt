# Generic Executor in Idris (via GADTS)

This is a variation of [`01_gen_exec`](../01_gen_exec).

How to run the end-to-end example:

```
$ idris --build gen_exec.ipkg
$ ./counter_test.exe
True
```

The main point of this exercise is to replace a notion of Protocol with GADTs, -
meaning (to some extent) that we couple the protocol (how the types of 
Call/Reply relate) and data types for messages into a single construct.

The names of the files/modules are preserved, so the reader can observe 
the differences via comparing corresponding files.

## Explicitness

This variant also doesn't try to "hide dependencies" needed for type checking -
and it passes types explicitly:

```idris
mkServer : Int -> Eff CounterServer [STATE Int]
mkServer initState =
  do put initState
     pure (MkCounterServer incImpl decImpl equalImpl closerImpl)
  where
    exec : GenExec.Executor Int Cast Call
    exec = GenExec.mkExecutor counterHandlers
    incImpl delta = execCast exec (Inc delta)
    decImpl delta = execCast exec (Dec delta)
    equalImpl i = execCall exec Bool (Equal i)
    closerImpl x y = execCall exec Int (Closer x y)
``` 

Put your attention to:

```idris
equalImpl i = execCall exec Bool (Equal i)
closerImpl x y = execCall exec Int (Closer x y)
```

`Bool` and `Int` are passed explicitly.

[Next episode](../03_gen_exec_gadt) will try to "hide dependencies".
