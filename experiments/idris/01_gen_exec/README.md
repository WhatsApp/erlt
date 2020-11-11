# Generic Executor in Idris

This is the simplest model of `gen_server` in idris.

How to run the end-to-end example:

```
$ idris --build gen_exec.ipgk
$ ./counter_test.exe
True
```

## [`GenExec`](GenExec.idr) 

`GenExec` is a simplification of `gen_server`.
It provides a simple infrastructure for execution of stateful computations
via [STATE](http://docs.idris-lang.org/en/latest/effects/summary.html#state).


In essense, given a `Handlers` record (= gen_server callbacks):

```idris
record Handlers (stateT : Type)
                (castT : Type)
                (callT: Type)
                (protocol : Protocol stateT castT callT) where
  handleCast : castT -> stateT -> stateT
  handleCall : (c : callT) -> stateT -> ((callReplyT protocol c), stateT)
```

it provides an `Executor`, which keeps the state internally:

```idris
record Executor (stateT : Type)
                (castT : Type)
                (callT : Type)
                (protocol : Protocol stateT castT callT) where
  execCast : castT -> Eff () [STATE stateT]
  execCall : (c : callT) -> Eff (callReplyT protocol c) [STATE stateT]
```

`Executor` handles `cast`s and `call`s by delegating execution of "business logic"
to provided `Handlers`. However `Executor` keeps the state internally - using
`STATE` resource.

On the surface `Executor` exposes the API similar to gen_server:

```idris
execCast : castT -> Eff () [STATE stateT]
execCall : (c : callT) -> Eff (callReplyT protocol c) [STATE stateT]
```

Ignoring effects in signatures (as it's done in Erlang/erlT/...) the API is read
similar to gen_server:

```idris
execCast : castT -> ()
execCall : (c : callT) -> (callReplyT protocol c)
```

Note that `handleCall` relies on dependent types, - since the type of the result
*depends* on the value of the request.  

## [Counter](Counter.idr)

The only interesting place is `handle_call`:

```idris
data Call = Equal Int | Closer Int Int

replyT : Call -> Type
replyT (Equal x) = Bool
replyT (Closer x y) = Int

handleCall : (c : Call) -> Int -> (replyT c, Int)
handleCall (Equal x) state =
  (x == state, state)
handleCall (Closer x y) state =
  (if abs (x - state) < abs (y - state) then x else y, state)
```

Corresponding `CounterServer` exposes user-friendly API:

```idris
record CounterServer where
  inc : () -> Eff () [STATE Int]
  dec : () -> Eff () [STATE Int]
  equal : Int -> Eff Bool [STATE Int]
  closer : Int -> Int -> Eff Int [STATE Int]
```

Again, ignoring effects in signatures, it becomes:

```idris
record CounterServer where
  inc : () -> ()
  dec : () -> ()
  equal : Int -> Bool
  closer : Int -> Int -> Int
```

This is how `Handlers` record is created:

```idris
counterHandlers = GenExec.MkHandlers handleCast handleCall
```

This is how CounterServer is instantiated:

```idris
mkServer initState =
  do put initState
     pure (MkCounterServer incImpl decImpl equalImpl closerImpl)
  where
    exec = GenExec.mkExecutor counterHandlers
    incImpl () = execCast exec Inc
    decImpl () = execCast exec Dec
    equalImpl i = execCall exec (Equal i)
    closerImpl x y = execCall exec (Closer x y)
``` 

## CounterTest

Take a look at `CounterTest.idr` for a simple working (runnable!) example.
