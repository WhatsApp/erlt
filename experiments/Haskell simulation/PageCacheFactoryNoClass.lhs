
This version of PageCacheFactory passes the callbacks expicitly using
a behaviour value, rather than a class instance. It is otherwise the
same as PageCacheFactory.lhs.

> {-# LANGUAGE GADTs, MultiParamTypeClasses #-}
> 
> module PageCacheFactory(State,PageCache,start,get,getAll,put) where
> 
> import Prelude hiding (init)
> import qualified Data.Map.Strict as Map
> import Processes
> import GenFactoryNoClass hiding (start)
> import qualified GenFactoryNoClass as GenFactory
> 
> behaviour =  Behaviour {
>   init_        = initPC,
>   handle_call_ = handleCall,
>   handle_cast_ = handleCast
>   }
>   
> type Page = String
> newtype State = State (Map.Map Integer String)
> 
> data CallRequest a where
>   Get    :: Integer ->         CallRequest (Maybe Page)
>   GetAll ::                    CallRequest [(Integer,Page)]
>   
> data CastRequest = Put Integer Page
> 
> newtype PageCache = PageCache (GenFactory CallRequest CastRequest)
> 
> initPC :: State -> Process State
> initPC = return
> 
> handleCall :: CallRequest a -> State -> Process (CallResponse State a)
> handleCall (Get i) (State s) = return $ Reply (Map.lookup i s) (State s)
> handleCall GetAll (State s) = return $ Reply (Map.toList s) (State s)
> 
> handleCast :: CastRequest -> State -> Process (CastResponse State)
> handleCast (Put i p) (State s) = return $ NoReply $ State $ Map.insert i p s
> 
> -- The API
> 
> start n initialState = PageCache <$> GenFactory.start behaviour (State initialState) n
> 
> get (PageCache server) key = call server (Get key)
> 
> getAll (PageCache server) = call server GetAll
> 
> put (PageCache server) key page = cast server (Put key page)
> 