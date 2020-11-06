This module provides a typed simulation of Erlang message passing. The
simulation does not cover links and exits. The interesting thing here
is that we can write Erlang-like code which is typed. The actual
*code* in this module is not so important--it doesn't correspond to
anything in erlt. It's the type of the API which matters.

All "Erlang-like code" uses the Process monad to allow
side-effects. This monad supports Haskell IO (including Concurrent
Haskell operations), and maintains a reference to the "self" Pid. In
erlt, this monad would not appear, so read the type 'Process a' as
'a'.

The API supported is

    send :: Typeable a => Pid a -> a -> Process a
    self :: Process (Pid a)
    spawn :: Process a -> Process (Pid b)
    receive :: Clause b -> Process b

Pids carry a type parameter, thus

    send :: Typeable a => Pid a -> a -> Process a

only allows messages of type 'a' to be sent to a Pid of type 'Pid
a'. When you have a Pid in your hand, in other words, you can only
send it messages of a single type. However, processes can *receive*
messages of any type. To support this, messages are embedded in
Haskell's Dynamic type in the mailbox. Because Haskell run-time
representations don't carry type information, then using Dynamic means
the compiler must *pass* type information around in the code. The
constraint Typeable a => tells the compiler to do so; clearly, such
constraints are not necessary in erlt. So the corresponding type for
send is just

    -spec send :: (Pid A, A) -> A

The type for self is just

    self :: Process (Pid a)

This allows a process to create a copy of its own Pid for receiving
*any type of message at all*. So when a process sends its own Pid to
another process, to later receive a reply, it can guarantee that the
reply will have the right type by calling self to create a Pid of the
corresponding type.

The type of spawn is

    spawn :: Process a -> Process (Pid b)

corresponding to

    -spec spawn :: (() -> A) -> Pid B

We give spawn the computation in the process body (which can return
any type at all), and we get back a Pid. (I added the '() ->' because
of course, the argument computation is performed in the newly spawned
process). Note that we can freely choose the type of Pid we get back!
This is to be expected, because the types don't track what type of
messages the processes *receive* (and indeed, there can be many
different types), but it does feel a little strange for small
processes that perhaps only receive one message. It is thus the
*caller's* responsibility to make sure that spawn is used to create a
Pid for the type of message that the process is actually expecting.

Finally,

    receive :: Clause b -> Process b

The argument of receive is a (combination of) clauses, according to
the following grammar:

    clauses ::= clause (|||| clause)*
    clause ::= guarded (\msg -> boolexpr --> expr)
            |  clause (\msg -> expr)
            |  defaultClause (\msg -> expr)

The first two alternatives take a *typed* message as an argument, and
will not match a message of a different type. The defaultClause
matches any message, and receives it with type Dynamic. The first
clause to match is chosen; if a mailbox contains several messages then
the first message to match any clause is chosen, as in Erlang.

A deficiency in the simulation: if a clause uses pattern matching, and
the match fails, then the code will just crash. It would be preferable
to go on to the next clause in that case. That's possible, but would
be a bit more work, and isn't necessary yet. So use receive with care.

Receive is inefficient: a blocked receive rescans the whole mailbox
every time a message is added. It doesn't matter for prototyping
types.

Examples:

    printer() -> spawn(fun() -> receive N -> io:format("~p\n",[N]) end).

becomes

    printer :: Process (Pid Integer)
    printer = spawn $ receive $ clause $ \n -> liftIO . putStrLn . show $ (n::Integer)

Note we have to specify a (monomorphic) type for the message--I chose Integer.

    bounce() -> spawn(fun() -> receive {Pid,A} -> Pid ! A end).

becomes

    bounce :: Process (Pid (Pid Integer,Integer))
    bounce = spawn $ receive $ clause $ \(pid,a) -> (send pid (a :: Integer))

Again, I had to choose the type of the message--even though I declared
bounce to be a process expecting a (Pid Integer, Integer)
message. This is because the type of message *received* is unrelated
to the type of Pid returned by *spawn*.

There are more extensive examples in GenServer.hs.

> -- TODO: the type of the result of spawn can be completely unrelated
> -- to the messages it receives. Especially when the process only has
> -- one receive in it, this is very annoying!
 
> {-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

> module Processes(
>   Pid, Process, Clause, runProcess, spawn, send, self, clause, guarded, (-->), defaultClause, (||||), receive,
>   Typeable, MonadIO(..)
>   ) where

> import Data.Dynamic             -- messages are of type Dynamic
> import Control.Concurrent
> import Control.Concurrent.MVar
> import Control.Monad.Reader
> import Control.Applicative
> import Control.Monad.Fail

> type Mailbox = MVar MailboxContents
> data MailboxContents = Queue [Dynamic] | Blocked (MVar Dynamic)

> newtype Pid a = Pid {unPid :: Mailbox}

> newtype Process a = Process {unProcess :: ReaderT Mailbox IO a}
>   deriving (Monad, Applicative, Functor, MonadFail, MonadIO)

> runProcess (Process m) = do
>   mailbox <- newMVar (Queue [])
>   runReaderT m mailbox 

> spawn :: Process a -> Process (Pid b)
> spawn (Process m) = Process $ liftIO $ do
>   mailbox <- newMVar (Queue [])
>   forkIO (runReaderT (m >> return ()) mailbox)
>   return (Pid mailbox)

> send :: Typeable a => Pid a -> a -> Process a
> send (Pid mailbox) a = Process $ liftIO $ do
>   q <- takeMVar mailbox
>   case q of
>     Queue as -> putMVar mailbox (Queue (as ++ [toDyn a]))
>     Blocked var -> putMVar var (toDyn a)
>   return a

> self :: Process (Pid a)
> self = Process $ Pid <$> ask

> newtype Clause b = Clause (Dynamic -> Maybe (Process b))

> clause :: Typeable a => (a -> Process b) -> Clause b
> clause body = guarded $ \a -> True --> body a

> guarded :: Typeable a => (a -> GuardedClause b) -> Clause b
> guarded test = Clause $ \dyn -> do
>   a <- fromDynamic dyn
>   let GuardedClause ok body = test a
>   guard ok
>   return body

> data GuardedClause a = GuardedClause Bool (Process a)

> (-->) = GuardedClause

> defaultClause body = Clause $ \dyn -> return (body dyn)

> (||||) :: Clause b -> Clause b -> Clause b
> Clause f |||| Clause g = Clause $ \dyn -> f dyn <|> g dyn

> -- TODO: Define receiveAfter0 (or generalize a clause to allow after0)
> receive :: Clause b -> Process b
> receive (Clause body) = Process $ do
>   mailbox <- ask
>   Queue q <- liftIO $ takeMVar mailbox
>   case tryReceive body q of
>     Just (q',m) -> do
>       liftIO $ putMVar mailbox (Queue q')
>       unProcess m
>     Nothing -> do
>       liftIO $ do
>         var <- newEmptyMVar
>         putMVar mailbox (Blocked var)
>         msg <- takeMVar var -- wait for a send
>         putMVar mailbox (Queue (q ++ [msg]))
>           -- quadratic! Could just check msg
>       unProcess $ receive (Clause body)

> tryReceive body [] = Nothing
> tryReceive body (msg:msgs) =
>       ((msgs,) <$> body msg)
>   <|> do (msgs',m) <- tryReceive body msgs; return (msg:msgs',m)