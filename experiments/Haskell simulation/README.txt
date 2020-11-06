This is a simulation of a typed genserver in Haskell. The files are
literate Haskell, containing a lot of explanation of the idea.

Start by reading the comments in Processes.lhs. This will show you how
Erlang message passing is simulated, so you know what the GenServer is
built on.

Then look at PageCache.lhs. This is the example client of the
GenServer, and it is here that the key idea is explained.

Finally take a look at GenServer.lhs to see how the server side
looks. Here you will also find an explanation of why rank N types are
needed (for one small part).
