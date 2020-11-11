This is a version of the page cache that uses a GenFactory instead of
a GenServer. It can be started with any number of workers. Since the
workers don't share state, this is a bit pointless--a request hits in
the cache only if it happens to go to the worker that received the
corresponding put. But it does illustrate the point that we can build
a GenFactory without problems.

Here's an example session, that illustrates requests going to
different workers. With only one worker, the cache behaves as
expected.

*PageCacheFactory> pc <- runProcess $ start 1 Map.empty
*PageCacheFactory> runProcess $ getAll pc
[]
*PageCacheFactory> runProcess $ put pc 1 "hello"
*PageCacheFactory> runProcess $ put pc 2 "world"
*PageCacheFactory> runProcess $ get pc 1
Just "hello"
*PageCacheFactory> runProcess $ getAll pc
[(1,"hello"),(2,"world")]

When we start the page cache with two workers, then some get requests
hit, and some miss.

*PageCacheFactory> pc <- runProcess $ start 2 Map.empty
*PageCacheFactory> runProcess $ put pc 1 "hello"
*PageCacheFactory> runProcess $ put pc 2 "world"
*PageCacheFactory> runProcess $ getAll pc
[(1,"hello")]
*PageCacheFactory> runProcess $ getAll pc
[(2,"world")]
*PageCacheFactory> runProcess $ getAll pc
[(1,"hello")]
*PageCacheFactory> runProcess $ getAll pc
[(2,"world")]

The implementation just defines the same behaviour as the ordinary
PageCache; the only differences are that we start a GenFactory, and
supply the number of workers to start.

> {-# LANGUAGE GADTs, MultiParamTypeClasses #-}

> module PageCacheFactory(State,PageCache,start,get,getAll,put) where

> import Prelude hiding (init)
> import qualified Data.Map.Strict as Map
> import Processes
> import GenFactory hiding (start)
> import qualified GenFactory

> instance Behaviour State CallRequest CastRequest where
>   init = initPC
>   handle_call = handleCall
>   handle_cast = handleCast

> type Page = String
> newtype State = State (Map.Map Integer String)

> data CallRequest a where
>   Get    :: Integer ->         CallRequest (Maybe Page)
>   GetAll ::                    CallRequest [(Integer,Page)]

> data CastRequest = Put Integer Page

> newtype PageCache = PageCache (GenFactory CallRequest CastRequest)

> initPC :: State -> Process State
> initPC = return

> handleCall :: CallRequest a -> State -> Process (CallResponse State a)
> handleCall (Get i) (State s) = return $ Reply (Map.lookup i s) (State s)
> handleCall GetAll (State s) = return $ Reply (Map.toList s) (State s)

> handleCast :: CastRequest -> State -> Process (CastResponse State)
> handleCast (Put i p) (State s) = return $ NoReply $ State $ Map.insert i p s

> -- The API

> start n initialState = PageCache <$> GenFactory.start (State initialState) n

> get (PageCache server) key = call server (Get key)

> getAll (PageCache server) = call server GetAll

> put (PageCache server) key page = cast server (Put key page)
