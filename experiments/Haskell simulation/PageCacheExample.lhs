> module PageCacheExample where

> import qualified Data.Map.Strict as Map
> import Processes
> import PageCache

> example = runProcess $ do
>   server <- start Map.empty
>   put server 1 "hello"
>   put server 2 "world"
>   getAll server >>= print
>   get server 1 >>= print
>   get server 2 >>= print
>   get server 3 >>= print
>   where print x = liftIO . putStrLn . show $ x