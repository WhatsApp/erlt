This module is a variation on the PageCache I wrote earlier; the only
difference is that it uses the NamedGenServer interface to start a
registered page cache, and thus its API need not be passed the page
cache pid. Since most of the code is the same, I only remark on the
places where it is different.

> {-# LANGUAGE GADTs, StandaloneDeriving #-}
> 
> module NamedPageCache where
> 
> import Prelude hiding (init)
> import qualified Data.Map.Strict as Map
> import Processes
> import GenServer hiding (start)
> import qualified GenServer
> import NamedGenServer
> 
> behaviour = Behaviour{init_=init,handle_call_=handleCall,handle_cast_=handleCast}
> 
> type Page = String
> type State = Map.Map Integer Page
> 
> data CallRequest a where
>   Get    :: Integer ->         CallRequest (Maybe Page)
>   GetAll ::                    CallRequest [(Integer,Page)]
> 
> data CastRequest = Put Integer Page

The unnamed version of this code defines a newtype PageCache, that
just wraps the genserver pid (of type GenServer CallRequest
CastRequest). This version of the code instead defines a *name* for
the page cache. Name types are GADTs with nullary constructors, whose
result types are parameterized on the type of thing they name. Thus
PageCache (defined below) is a name that can be registered for a
GenServer CallRequest CastRequest--that is, the pid we need to store
in the registry.

> data PageCache a where
>   PageCache :: PageCache (GenServer CallRequest CastRequest)

The registry code needs to be able to compare names, which in Haskell
requires an Eq instance, so we just derive the standard one. This
would not be necessary in Erlang.

> deriving instance Eq (PageCache a)

...and back to the same code as before.

> init :: State -> Process State
> init = return
> 
> handleCall :: CallRequest a -> State -> Process (CallResponse State a)
> handleCall (Get i) s = return $ Reply (Map.lookup i s) s
> handleCall GetAll s = return $ Reply (Map.toList s) s
> 
> handleCast :: CastRequest -> State -> Process (CastResponse State)
> handleCast (Put i p) s = return $ NoReply $ Map.insert i p s

The API operations are of course different from before, in that we no
longer need to return a pid from the start function, or pass the
server pid to the operations. Instead they use the NamedGenServer
operations, to register the page cache with the given name, and then
call the server with that name. Here we just pass in the name
PageCache declared above, in the same way that today's Erlang code
would pass in an atom.

> -- ********** THE API *********************************************************
> 
> start initialState = startNamed PageCache behaviour initialState
> 
> get key = callNamed PageCache (Get key)
> 
> getAll = callNamed PageCache GetAll
> 
> put key page = castNamed PageCache (Put key page)
> 
> {-
> The types can be derived by type inference. They are as expected:
> 
> start  :: State           -> Process ()
> get    :: Integer         -> Process (Maybe Page)
> getAll ::                    Process [(Integer, Page)]
> put    :: Integer -> Page -> Process ()
> 
> Here's an example of usage. The only difference from the unnamed page
> cache is that there is no need to save the pid returned by start, and
> pass it to the other functions.
> 
> *NamedPageCache> runProcess $ start Map.empty
> *NamedPageCache> runProcess $ put 1 "hello"
> *NamedPageCache> runProcess $ put 2 "clouds"
> *NamedPageCache> runProcess $ getAll
> [(1,"hello"),(2,"clouds")]
> *NamedPageCache> runProcess $ get 1
> Just "hello"
> *NamedPageCache> runProcess $ get 2
> Just "clouds"
> *NamedPageCache> runProcess $ get 3
> Nothing
> 
> -}