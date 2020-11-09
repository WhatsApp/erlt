||| Infrastructure for execution of stateful computations
||| via simple Effects and Resources
module GenExec

import Effects
import Effect.State

public export
record Protocol (stateT : Type)
                (castT : Type)
                (callT : Type) where
  constructor MkProtocol
  callReplyT : callT -> Type

public export
record Handlers (stateT : Type)
                (castT : Type)
                (callT: Type)
                (protocol : Protocol stateT castT callT) where
  constructor MkHandlers
  handleCast : castT -> stateT -> stateT
  handleCall : (c : callT) -> stateT -> ((callReplyT protocol c), stateT)


public export
record Executor (stateT : Type)
                (castT : Type)
                (callT : Type)
                (protocol : Protocol stateT castT callT) where
  constructor MkExecutor
  execCast : castT -> Eff () [STATE stateT]
  execCall : (c : callT) -> Eff (callReplyT protocol c) [STATE stateT]

export
mkExecutor : {stateT : Type} ->
             {castT : Type} ->
             {callT : Type} ->
             {protocol : Protocol stateT castT callT} ->
             (Handlers stateT castT callT protocol) ->
             (Executor stateT castT callT protocol)
mkExecutor handlers =
  MkExecutor sCast sCall
  where
    sCast cast =
      do state1 <- get
         let state2 = (handleCast handlers cast state1)
         put state2
         pure ()
    sCall call =
      do state1 <- get
         let (reply, state2) = (handleCall handlers call state1)
         put state2
         pure reply
