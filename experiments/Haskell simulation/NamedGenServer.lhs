This module just adds functionality for named genservers to the old
GenServer module: it provides startNamed, callNamed and castNamed. The
latter two functions take the registered name as an argument instead
of the GenServer itself, and the first function registers the
GenServer with the given name, rather than returning it.

> module NamedGenServer(startNamed, callNamed, castNamed) where

> import Processes
> import Registry
> import GenServer

> startNamed name b s = do
>   gs <- start b s
>   register name gs

> callNamed name c = do
>   Just gs <- whereis name
>   call gs c

> castNamed name c = do
>   Just gs <- whereis name
>   cast gs c

