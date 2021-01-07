module Main where

import qualified ELP (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  ELP.someFunc
