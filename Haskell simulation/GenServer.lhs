This module is a fully typed *implementation* of a (very simple)
generic server. Read the example client, PageCache.ls, first. The main
idea is explained there.

This code requires not only GADTs, but also the RankNTypes extension.

> {-# LANGUAGE GADTs, RankNTypes #-}

> module GenServer(Behaviour(..), GenServer, CallResponse(..), CastResponse(..),
>                  start, call, cast) where
 
> import Processes

Callers need to supply a suitable behaviour. Since Haskell can't pass
modules as parameters in the way Erlang can, then I have defined a
type of genserver behaviours. Client modules construct a value of this
type, containing the call-back functions they are providing, and pass
it to the generic server.

**This is where the RankNType extension is needed**. The key idea in
this simulation is to give handle_call a *polymorphic* type, so that
it can be used by the generic server to compute responses of different
types. So the package of call-backs we give to the generic server must
contain a polymorphic function. Look at the type of the handle_call_
field in this definition. It is polymorphic. That's why rank N types
are needed. Conceivably we might provide a different mechanism for
passing in the callbacks, in which case rank N types would not be
needed.

> data Behaviour state call cast =
>   Behaviour{
>     init_        :: state -> Process state,
>     handle_call_ :: forall a. Typeable a => 
>                       call a -> state -> Process (CallResponse state a),
>     handle_cast_ :: cast -> state -> Process (CastResponse state)
>     }

Here is the type of the generic server itself: it's just a Pid expecting requests.

> newtype GenServer call cast = GenServer (Pid (Request call cast))

Requests to the GenServer are either calls or casts. Because Requests
are sent in messages, they need to be 'Typeable' for Haskell Dynamics
to work; hence Call as a Typeable constraint that causes Haskell to
store the type of a in the request itself. This would not appear in an
Erlang version of the code.

**The key thing here**: Request is a GADT, which is used here to
support an existential type--a Call Request can contain a client-level
'call a' for *any* result type 'a'. The type-checker will ensure that
the Pid supplied along with it is used ONLY to send back a response of
the correct type.

> data Request call cast where
>   Call :: Typeable a => call a -> Pid a -> Request call cast
>   Cast ::               cast            -> Request call cast

These types are exported, and used by client code to construct
responses in handle_call and handle_cast respectively.

> data CallResponse s a = Reply a s
> data CastResponse s = NoReply s

The implementation itself. All the type variables need Typeable
constraints for the sake of Haskell dynamics; none of these would be
needed in typed Erlang.

> start :: (Typeable call, Typeable cast) =>
>            Behaviour state call cast -> state -> Process (GenServer call cast)
> start b s = fmap GenServer . spawn $ do
>   s' <- init_ b s
>   serverLoop b s'

> serverLoop :: (Typeable call, Typeable cast) =>
>                 Behaviour state call cast -> state -> Process ()
> serverLoop b s = receive $ clause $ \r -> case r of
>   Call c client -> do Reply a s' <- handle_call_ b c s
>                       send client a
>                       serverLoop b s'
>   Cast c        -> do NoReply s' <- handle_cast_ b c s
>                       serverLoop b s'

> call :: (Typeable a, Typeable call, Typeable cast) =>
>           GenServer call cast -> (call a) -> Process a
> call (GenServer pid) ca = do
>   me <- self
>   -- a deficiency: requests do not carry a unique reference
>   send pid (Call ca me)
>   receive $ clause $ return

> cast :: (Typeable call, Typeable cast) =>
>           GenServer call cast -> cast -> Process ()
> cast (GenServer pid) ca = do
>   send pid (Cast ca)
>   return ()