module Counter

import Effects
import Effect.State
import GenExec

data Cast = Inc | Dec

handleCast : Cast -> Int -> Int
handleCast Inc state = state + 1
handleCast Dec state = state - 1

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
  inc : () -> Eff () [STATE Int]
  dec : () -> Eff () [STATE Int]
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
    incImpl () = execCast exec Inc
    decImpl () = execCast exec Dec
    equalImpl i = execCall exec (Equal i)
    closerImpl x y = execCall exec (Closer x y)
