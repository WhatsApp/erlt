This module is a fully typed *implementation* of a (very simple)
generic server. Read the example client, PageCache.ls, first. The main
idea is explained there.

This variant encodes the callbacks as an instance of a type class, an
idea taken from Hamler
(https://www.emqx.io/news/hamler-0-2-otp-behaviours-with-type-classes).

This code requires not only GADTs, but also the RankNTypes extension.

> {-# LANGUAGE GADTs, MultiParamTypeClasses, FunctionalDependencies #-}

> module GenServerClass(Behaviour(..), GenServer, CallResponse(..), CastResponse(..),
>                       start, call, cast) where

> import Prelude hiding (init)
> import Processes

> class (Typeable call, Typeable cast) => Behaviour state call cast
>     | state -> call cast, call -> state, cast -> state where
>   init :: state -> Process state
>   handle_call :: Typeable a =>
>                    call a -> state -> Process (CallResponse state a)
>   handle_cast :: cast -> state -> Process (CastResponse state)

Callers supply callback functions by defining a suitable instance of
this class. Note: there is a choice between using functional
dependencies and using type families, but in any case, we need to be
able to determine the instance just from the type of the state,
because that is all that appears in the type of init. This forces the
state type to be different for every gen_server, so it will often be a
newtype.

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

> start :: Behaviour state call cast => state -> Process (GenServer call cast)
> start s = fmap GenServer . spawn $ do
>   s' <- init s
>   serverLoop s'

> serverLoop :: Behaviour state call cast => state -> Process ()
> serverLoop s = receive $ clause $ \r -> case r of
>   Call c client -> do Reply a s' <- handle_call c s
>                       send client a
>                       serverLoop s'
>   Cast c        -> do NoReply s' <- handle_cast c s
>                       serverLoop s'

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