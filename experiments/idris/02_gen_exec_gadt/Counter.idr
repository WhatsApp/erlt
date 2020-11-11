module Counter

import Effects
import Effect.State
import GenExec

data Cast = Inc Int | Dec Int

handleCast : Cast -> Int -> Int
handleCast (Inc delta) state = state + delta
handleCast (Dec delta) state = state - delta

data Call : (c : Type) -> Type where
  Equal : Int -> Call Bool
  Closer : Int -> Int -> Call Int

handleCall : (a : Type) -> (Call a) -> Int -> (a, Int)
handleCall _ (Equal x) state =
  (x == state, state)
handleCall _ (Closer x y) state =
  (if abs (x - state) < abs (y - state) then x else y, state)


counterHandlers : Handlers Int Cast Call
counterHandlers = GenExec.MkHandlers handleCast handleCall

public export
record CounterServer where
  constructor MkCounterServer
  inc : Int -> Eff () [STATE Int]
  dec : Int -> Eff () [STATE Int]
  equal : Int -> Eff Bool [STATE Int]
  closer : Int -> Int -> Eff Int [STATE Int]

export
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
