This module illustrates one way of typing the registry, using
GADTs. I use the StandaloneDeriving extension also, just to be able to
derive the default equality for GADTs.

> {-# LANGUAGE GADTs, StandaloneDeriving #-}

> module Registry where

> import Data.Typeable
> import Control.Concurrent.MVar
> import System.IO.Unsafe

> import Processes

The basic idea is this: since we need to be able to register
pids of different types, then register and whereis must have
polymorphic types... and we must be able to determine the type of the
pid from the (type of the) name.

Thus, instead of using atoms as names, I use values of the type Name
a, where a name of type Name a names something of type a in the
registry. In fact, the three core operations have the types:

> register   :: (Typeable n, Typeable a, Eq (n a)) => n a -> a -> Process ()
> whereis    :: (Typeable n, Typeable a, Eq (n a)) => n a -> Process (Maybe a)
> unregister :: (Typeable n, Typeable a, Eq (n a)) => n a -> Process ()

Remember that the constraints, and the Process type, would not appear
in Erlang--so these type correspond to

register   :: n a -> a -> ()
whereis    :: n a -> Maybe a
unregister :: n a -> ()

Notice that the Name type doesn't appear! One might have expected to
see Name instead of n in the types above. That is because Names must
be defined in many different modules, and Haskell does not allow
extensible data types. So instead of using a single Name type, which
each module would extend with new Name constructors, I allow each
module to define its own type of names... which means that the
registry functions need to be polymorphic not only in the variable
a--the type of value that the name actually names--but also in the
name type itself.

The idea is that each module that uses the registry defines its own
name type, as a GADT, so that each constructor (name) can be assigned
its own corresponding type in the registry. See NamedPageCache.lhs for
a simple example of this.

In Erlang, then the names must of course be of a type whose
representation is a single atom, and the values stored in the registry
must be represented as pids (although they may well be newtypes/opaque
types which contain pids). There is no check for this in this Haskell
simulation; it could be achieved by adding constraints IsPid and
IsAtom, and giving types such as

register :: (IsAtom (n a), IsPid a) => n a -> a -> ()

The code below just implements the registry API with these types. It
is type-safe, although I did need to use casts here and there. There
is no possibility of these casts failing, though--and in any case,
this code corresponds to C code in the Erlang VM, so there is no real
need for it to be typeable.

The registry itself is a global MVar containing a list of registry
entries. (Creating a global variable like this, using unsafePerformIO,
is a really horrible hack. But it suffices for making this prototype).

> registry :: MVar [RegEntry]
> registry = unsafePerformIO $ newMVar []

Entries in the registry need to be of an existential type, because the
pids in the registry are of different types. They are essentially
name/pid-pairs. However, the type of the pid follows from the type of
the name.

> data RegEntry where
>   RegEntry :: (Typeable n, Typeable a) => n a -> a -> RegEntry

Now the three core operations, which read and write the global
variable, but delegate most of the work to a couple of helper
functions. Note that whereis is able to return a result whose type is
determined by the type of the name!

> register n a = liftIO $ do
>   rs <- takeMVar registry
>   case findRegEntry n rs of
>     Nothing ->
>       putMVar registry (RegEntry n a : rs)
>     Just _ -> do
>       putMVar registry rs
>       error "register: badarg"

> whereis n = liftIO $ do
>   rs <- takeMVar registry
>   putMVar registry rs
>   return $ case findRegEntry n rs of
>     Nothing ->
>       Nothing
>     Just (RegEntry n' a) ->
>       case cast n' of
>         Just n'' | n==n'' ->
>           cast a
>         _ -> error "The names were not equal!"

> unregister n = liftIO $ do
>   rs <- takeMVar registry
>   case findRegEntry n rs of
>     Nothing -> do
>       putMVar registry rs
>       error "unregister: badarg"
>     Just _ -> do
>       putMVar registry (delRegEntry n rs)

The functions that actually find and delete registry entries. These
just search for a reg entry containing a given name, and use Haskell
cast to compare the name in each reg entry with the one we're looking
for... if they are of the same type, then we compare their values,
while if they are of different types then cast returns Nothing, and
they must be different.

> findRegEntry name [] = Nothing
> findRegEntry name (RegEntry name' a:rs) =
>   case cast name' of
>     Just name'' | name==name'' ->
>       Just (RegEntry name' a)
>     _ ->
>       findRegEntry name rs

> delRegEntry name [] = []
> delRegEntry name (RegEntry name' a:rs) =
>   case cast name' of
>     Just name'' | name==name'' ->
>       rs
>     _ ->
>       (RegEntry name' a):delRegEntry name rs

