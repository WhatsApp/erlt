This module defines a "GenFactory" behaviour. It is similar to (and
derived from GenServerClass), which is a GenServer using GADTs that
takes the callbacks as an instance of the Behaviour class. Instead of
starting just one worker, though, GenFactory starts a configurable
number of workers, each with their own state, and routes requests to
the next free worker.

I'll comment only the new parts of the code below.

> {-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables #-}

> module GenFactory(Behaviour(..), GenFactory, CallResponse(..), CastResponse(..),
>                   start, call, cast) where
                   
> import Prelude hiding (init)
> import Processes

> class (Typeable call, Typeable cast) => Behaviour state call cast
>     | state -> call cast where
>   init :: state -> Process state
>   handle_call :: Typeable a =>
>                    call a -> state -> Process (CallResponse state a)
>   handle_cast :: cast -> state -> Process (CastResponse state)

I renamed the GenServer type to GenFactory, but otherwise it is the
same.

> newtype GenFactory call cast = GenFactory (Pid (Request call cast))

> data Request call cast where
>   Call :: Typeable a => call a -> Pid a -> Request call cast
>   Cast ::               cast            -> Request call cast

> data CallResponse s a = Reply a s
> data CastResponse s = NoReply s

The state of the main 'server' process, which receives all the
requests sent to the factory. It keeps track of the available workers
(who are ready to be sent a request), and of the requests which have
not yet been assigned a worker.

> data Factory call cast = Factory {
>   workers :: [Pid (Request call cast)],
>   pending :: [Request call cast]
>   }

Here's how we start the server. It just starts up n workers, each of
which has a reference to the server process, and then starts the
main loop. The type signature is necessary here, as is the type
attached to the pending list in the argument of serverLoop. We're
creating the factory state for the first time here, and GHC *needs* to
know what type it is, so it knows which behaviour to pass to
serverLoop (to satisfy the Behaviour constraint).

> start :: forall call cast state.
>   Behaviour state call cast => state -> Int -> Process (GenFactory call cast)
> start s n = fmap GenFactory . spawn $ do
>   me <- self
>   ws <- sequence [startWorker me s | _ <- [1..n]]
>   serverLoop Factory{ workers=ws, pending=[] :: [Request call cast]}

The main server loop. The receive here can receive two types of
messages, which is why there are two clauses. The first type is a
Request, which is either forwarded to an available worker, or saved in
the state. The second type is a worker pid--when a worker is ready for
more work, it sends its pid back to the server, which sends it more
work, or reinserts it into the list of available workers if no work is
available.

> serverLoop :: (Typeable call, Typeable cast) => Factory call cast -> Process ()
> serverLoop factory = receive $
>     clause ( \r -> case factory of
>       Factory{workers=[]}   -> serverLoop factory{pending = pending factory++[r]}
>       Factory{workers=w:ws} -> do
>         send w r
>         serverLoop factory{workers = ws})
>     ||||
>     clause ( \w -> case factory of
>       Factory{pending=[]}   -> serverLoop factory{workers = workers factory ++ [w]}
>       Factory{pending=r:rs} -> do
>         send w r
>         serverLoop factory{pending=rs})

The code to start a worker, and the main worker loop. Workers just
serve requests, sending the result back to the original client, and
are very similar to the old GenServer process... the only difference
is that every time a worker finishes processing a request, it sends
its pid back to the server so that it can be sent another.

> startWorker :: (Behaviour state call cast, Typeable a) =>
>                  Pid (Pid a) -> state -> Process (Pid a)
> startWorker parent s = spawn $ do
>   s' <- init s
>   workerLoop parent s'

It's the workers that call handle_call, but of course they don't
pattern match on the call request, so they don't use the
"GADT"-ness. The Request type itself is a GADT because it is an
existential type, so the types of c and client involve rigid type
variables--this is no different from the original GADT-based
GenServer.

> workerLoop parent s = receive $ clause $ \r -> do
>   s' <- case r of
>           Call c client -> do Reply a s' <- handle_call c s
>                               send client a
>                               return s'
>           Cast c        -> do NoReply s' <- handle_cast c s
>                               return s'
>   me <- self
>   send parent me
>   workerLoop parent s'
                  
> call :: (Typeable a, Typeable call, Typeable cast) =>
>           GenFactory call cast -> (call a) -> Process a
> call (GenFactory pid) ca = do
>   me <- self
>   send pid (Call ca me)
>   receive $ clause $ return

> cast :: (Typeable call, Typeable cast) =>
>           GenFactory call cast -> cast -> Process ()
> cast (GenFactory pid) ca = do
>   send pid (Cast ca)
>   return ()