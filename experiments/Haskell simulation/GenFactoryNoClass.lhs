This is an alternative version of GenFactory that does not use a class
instance to pass the behaviour to the factory... instead, the
callbacks are encapsulated in a data type, in the same way as in
GenServer.lhs. As a result, this version needs the same type system
extensions as GenServer.lhs. GenFactory.lhs, on the other hand, uses a
class, borrowing the approach that Hamler uses to supply
callbacks. That's why it needs further language extensions.

The code is otherwise unchanged.

> {-# LANGUAGE GADTs, RankNTypes #-}
> 
> module GenFactoryNoClass(Behaviour(..), GenFactory, CallResponse(..), CastResponse(..),
>                          start, call, cast) where
>                   
> import Prelude hiding (init)
> import Processes
> 
> data Behaviour state call cast = Behaviour {
>   init_        :: state -> Process state,
>   handle_call_ :: forall a. Typeable a =>
>                     call a -> state -> Process (CallResponse state a),
>   handle_cast_ :: cast -> state -> Process (CastResponse state)
>   }
> 
> newtype GenFactory call cast = GenFactory (Pid (Request call cast))
> 
> data Request call cast where
>   Call :: Typeable a => call a -> Pid a -> Request call cast
>   Cast ::               cast            -> Request call cast
>   
> data CallResponse s a = Reply a s
> data CastResponse s = NoReply s
> 
> data Factory call cast = Factory {
>   workers :: [Pid (Request call cast)],
>   pending :: [Request call cast]
>   }
>   
> start :: (Typeable call, Typeable cast) =>
>   Behaviour state call cast -> state -> Int -> Process (GenFactory call cast)
> start b s n = fmap GenFactory . spawn $ do
>   me <- self
>   ws <- sequence [startWorker b me s | _ <- [1..n]]
>   serverLoop Factory{ workers=ws, pending=[] }
>   
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
>         
> startWorker :: (Typeable call, Typeable cast) =>
>                  Behaviour state call cast -> Pid (Pid (Request call cast)) -> state ->
>                    Process (Pid (Request call cast))
> startWorker b parent s = spawn $ do
>   s' <- init_ b s
>   workerLoop b parent s'
>   
> workerLoop b parent s = receive $ clause $ \r -> do
>   s' <- case r of
>           Call c client -> do Reply a s' <- handle_call_ b c s
>                               send client a
>                               return s'
>           Cast c        -> do NoReply s' <- handle_cast_ b c s
>                               return s'
>   me <- self
>   send parent me
>   workerLoop b parent s'
>   
> call :: (Typeable a, Typeable call, Typeable cast) =>
>           GenFactory call cast -> (call a) -> Process a
> call (GenFactory pid) ca = do
>   me <- self
>   send pid (Call ca me)
>   receive $ clause $ return
>   
> cast :: (Typeable call, Typeable cast) =>
>           GenFactory call cast -> cast -> Process ()
> cast (GenFactory pid) ca = do
>   send pid (Cast ca)
>   return ()