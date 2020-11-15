# Generic Server in Idris

This is a simple working model of `gen_server` in Idris. It is built upon the notion of 
[Process](https://github.com/idris-lang/Idris-dev/blob/master/libs/contrib/System/Concurrency/Process.idr) -
which is very close to Erlang processes.

How to run the end-to-end example:

```
$ idris --build gen_server.ipkg
$ ./gen_server.exe
True
```

## Req type

This code is in essence more elaborated [GenExec](../01_gen_exec).

In order to communicate via GenServer, - the `Req` type is introduced.

The interesting thing is that the `Req` type parameterised
by the `protocol` being handled. - Similar to how the `Vect` type is 
parameterised by the length of a vector. 

```idris
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
``` 

(A clarification: this type is not GADT.)

## GenServer API

Interaction with a GenServer is about execution I/O actions (communicating to 
other processes), - so I/O actions are part of its API: 

```
public export
record GenServer (stateT   : Type)
                 (castT    : Type)
                 (callT    : Type)
                 (protocol : Protocol stateT castT callT) where
  constructor MkGenServer
  cast : castT -> IO ()
  call : (c : callT) -> IO (callReplyT protocol c)
```

## Well-type server loop

```
serve handlers = loop where
  loop state1 = do {msg <- recv; handle msg} where
    handle (Cast cast) =
      let state2 = handleCast handlers cast state1
      in  loop state2
    handle (Call call replyTo) = do
      let (reply, state2) = handleCall handlers call state1
      Lift $ run $ send replyTo reply
      loop state2
```

## Well-typed call/cast internals

Handling of casts and calls happens in the current process/context, - via sending
messages to the spawned GenServer's process. 

```
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
```
