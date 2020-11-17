module Counter

import GenServer

data Cast = Inc Int | Dec Int

handleCast : Cast -> Int -> Int
handleCast (Inc delta) state = state + delta
handleCast (Dec delta) state = state - delta

data Call = Equal Int | Closer Int Int

replyT : Call -> Type
replyT (Equal _) = Bool
replyT (Closer _ _) = Int

handleCall : (c : Call) -> Int -> (replyT c, Int)
handleCall (Equal x) state =
  (x == state, state)
handleCall (Closer x y) state =
  (if abs (x - state) < abs (y - state) then x else y, state)

CounterProtocol : GenServer.Protocol Int Cast Call
CounterProtocol = GenServer.MkProtocol replyT

counterHandlers : Handlers _ _ _ CounterProtocol
counterHandlers = GenServer.MkHandlers handleCast handleCall

public export
record CounterServer where
  constructor MkCounterServer
  inc : Int -> IO ()
  dec : Int -> IO ()
  equal : Int -> IO Bool
  closer : Int -> Int -> IO Int

export
spawn : Int -> IO CounterServer
spawn initState =
  GenServer.spawn counterHandlers initState >>= pure . mkCounterServer where
    mkCounterServer gs = MkCounterServer incImpl decImpl equalImpl closerImpl
    where
      incImpl delta  = cast gs (Inc delta)
      decImpl delta  = cast gs (Dec delta)
      equalImpl i    = call gs (Equal i)
      closerImpl x y = call gs (Closer x y)
