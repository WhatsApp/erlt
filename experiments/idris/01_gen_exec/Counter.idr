module Counter

import Effects
import Effect.State
import GenExec

data Cast = Inc Int | Dec Int

handleCast : Cast -> Int -> Int
handleCast (Inc delta) state = state + delta
handleCast (Dec delta) state = state - delta

data Call = Equal Int | Closer Int Int

replyT : Call -> Type
replyT (Equal x) = Bool
replyT (Closer x y) = Int

handleCall : (c : Call) -> Int -> (replyT c, Int)
handleCall (Equal x) state =
  (x == state, state)
handleCall (Closer x y) state =
  (if abs (x - state) < abs (y - state) then x else y, state)

CounterProtocol : GenExec.Protocol Int Cast Call
CounterProtocol = GenExec.MkProtocol replyT

counterHandlers : Handlers _ _ _ CounterProtocol
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
    exec : GenExec.Executor _ _ _ CounterProtocol
    exec = GenExec.mkExecutor counterHandlers
    incImpl delta = execCast exec (Inc delta)
    decImpl delta = execCast exec (Dec delta)
    equalImpl i = execCall exec (Equal i)
    closerImpl x y = execCall exec (Closer x y)
