
> {-# LANGUAGE GADTs #-}

This module shows how a generic server client module can be written in
a fully-typed way, by implementing the page cache example from
https://erl2.quip.com/R9HMABh1sMm8 in Haskell, using simulated Erlang
message passing.

The two major problems in typing generic servers are the registry,
which tracks no type information on the processes it contains, and the
handle_call function which clients define, which has no Hindley-Milner
type (when different requests return different types). This code does
*not* address the registry problem; instead it shows how GADTS can be
used to type handle_call.

The key idea is this. The *problem* in typing handle_call is that the
branches of a case expression (or the separate function clauses) must
be allowed to return different types--each request that is handled may
have a different type of result. *But this is exactly the purpose of
GADTs*! Indeed, GADTs may be regarded as the minimal extension needed
to support this. Given this key idea, the rest is just putting it
together the right way.

> module PageCache(State,PageCache,start,get,getAll,put) where

> import Prelude hiding (init)
> import qualified Data.Map.Strict as Map
> import Processes
> import GenServer hiding (start)
> import qualified GenServer

First of all: we have to implement a generic server behaviour. In
Haskell we can't pass a module name to give access to the behaviour
functions; instead, we package them into a data structure. This
Behaviour type is defined in GenServer.hs; we just fill the fields
with the corresponding functions. Perhaps we can find a nicer way of
doing this in erlt.

> behaviour = Behaviour{init_=init,handle_call_=handleCall,handle_cast_=handleCast}

We define type alises for page contents and the page server state.

> type Page = String
> type State = Map.Map Integer String

This is the key step: the type of requests is a GADT, parameterised on
the result type that the call will return. It is this type that
permits the definition of handleCall with a different result type for
each request.

> data CallRequest a where
>   Get    :: Integer ->         CallRequest (Maybe Page)
>   GetAll ::                    CallRequest [(Integer,Page)]

So a CallRequest (Maybe Page) can only be a Get request, and likewise
a CallRequest [(Integer,Page)] can only be GetAll. Other types, such
as CallRequest Integer, are valid types, but there are no values of
those types.

Casts are no problem, because there is no result: the cast request is
a normal datatype.

> data CastRequest = Put Integer Page

The page cache itself is a generic server, which is parameterised on
the types of call and cast requests. Notice that GenServer is
parameterized not on the type of any *particular* call request, *but
on the parameterised type itself*. Its kind is (*->*) -> * -> *, so
doing this requires type variables of kinds other than *.

> newtype PageCache = PageCache (GenServer CallRequest CastRequest)

Start the server, given an initial state.

> init :: State -> Process State
> init = return

Here is the tricky handleCall function. The result is a CallResponse
(a type defined by the GenServer, which in this case just has a Reply
constructor). Remember, a request of type 'CallRequest a' should send
back a reply of type 'a'. But because CallRequest is a GADT, then in
the first clause 'a' is 'Maybe Page', while in the second clause it is
'[(Integer,Page)]'. So the type-checker ensures that the first clause is a

    Process (CallResponse State (Maybe Page))

while the second clause is a

    Process (CallResponse State [(Integer,Page)])

Typing problem solved.

> handleCall :: CallRequest a -> State -> Process (CallResponse State a)
> handleCall (Get i) s = return $ Reply (Map.lookup i s) s
> handleCall GetAll s = return $ Reply (Map.toList s) s

Handling casts poses no particular problem.

> handleCast :: CastRequest -> State -> Process (CastResponse State)
> handleCast (Put i p) s = return $ NoReply $ Map.insert i p s

********** THE API *********************************************************

These are the API functions, which use GenServer.call and GenServer.cast.

> start initialState = PageCache <$> GenServer.start behaviour initialState

> get (PageCache server) key = call server (Get key)

> getAll (PageCache server) = call server GetAll

> put (PageCache server) key page = cast server (Put key page)

The types can be derived by type inference. They are as expected:

start  :: State                        -> Process PageCache
get    :: PageCache -> Integer         -> Process (Maybe Page)
getAll :: PageCache                    -> Process [(Integer, Page)]
put    :: PageCache -> Integer -> Page -> Process ()

Here's an example of usage:

*PageCache> server <- runProcess $ start Map.empty
*PageCache> runProcess $ put server 1 "hello"
*PageCache> runProcess $ put server 2 "world"
*PageCache> runProcess $ getAll server
[(1,"hello"),(2,"world")]
*PageCache> runProcess $ get server 1
Just "hello"
*PageCache> runProcess $ get server 2
Just "world"
*PageCache> runProcess $ get server 3
Nothing

