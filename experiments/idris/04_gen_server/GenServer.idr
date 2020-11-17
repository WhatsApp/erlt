||| Infrastructure for execution of stateful computations
||| via System.Concurrency.Process
module GenServer

import System.Concurrency.Process

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

data Req : {stateT   : Type} ->
           {castT    : Type} ->
           {callT    : Type} ->
           (protocol : Protocol stateT castT callT) ->
           Type where
  Cast : castT ->
         Req {stateT} {castT} {callT} protocol
  Call : (c : callT) ->
         ProcID (callReplyT protocol c) ->
         Req {stateT} {castT} {callT} protocol

serve : {stateT   : Type} ->
        {castT    : Type} ->
        {callT    : Type} ->
        {protocol : Protocol stateT castT callT} ->
        Handlers stateT castT callT protocol ->
        stateT ->
        Process (Req protocol) ()
serve handlers = loop where
  loop state1 = do {msg <- recv; handle msg} where
    handle (Cast cast) =
      let state2 = handleCast handlers cast state1
      in  loop state2
    handle (Call call replyTo) = do
      let (reply, state2) = handleCall handlers call state1
      Lift $ run $ send replyTo reply
      loop state2

public export
record GenServer (stateT   : Type)
                 (castT    : Type)
                 (callT    : Type)
                 (protocol : Protocol stateT castT callT) where
  constructor MkGenServer
  cast : castT -> IO ()
  call : (c : callT) -> IO (callReplyT protocol c)

export
spawn : {stateT : Type} ->
        {castT : Type} ->
        {callT : Type} ->
        {protocol : Protocol stateT castT callT} ->
        Handlers stateT castT callT protocol ->
        stateT ->
        IO (GenServer _ _ _ protocol)
spawn handlers st =
  do {gsPid <- run $ create $ serve handlers st; pure (server gsPid)} where
    server pid = MkGenServer gCast gCall where
      gCast cast = do
        run $ send pid $ Cast cast
        pure ()
      gCall call = do
        me <- run myID
        run $ send pid $ Call call me
        run recv
