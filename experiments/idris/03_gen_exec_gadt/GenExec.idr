||| Infrastructure for execution of stateful computations
||| via simple Effects and Resources
module GenExec

import Effects
import Effect.State

public export
record Handlers (stateT : Type)
                (castT : Type)
                (callT: Type -> Type) where
  constructor MkHandlers
  handleCast : castT -> stateT -> stateT
  handleCall : {a : Type} -> (callT a) -> stateT -> (a, stateT)

public export
record Executor (stateT : Type)
                (castT : Type)
                (callT : Type -> Type) where
  constructor MkExecutor
  execCast : castT -> Eff () [STATE stateT]
  execCall : {a : Type} -> (callT a) -> Eff a [STATE stateT]

export
mkExecutor : {stateT : Type} ->
             {castT : Type} ->
             {callT : Type -> Type} ->
             (Handlers stateT castT callT) ->
             (Executor stateT castT callT)
mkExecutor handlers =
  MkExecutor sCast sCall
  where
    sCast cast =
      do state1 <- get
         let state2 = (handleCast handlers cast state1)
         put state2
         pure ()
    sCall : {a : Type}  -> (callT a) -> Eff a [STATE stateT]
    sCall call =
      do state1 <- get
         let (reply, state2) = (handleCall handlers call state1)
         put state2
         pure reply
