module Main
import Counter

import Counter
import Effects
import Effect.State

testCounterServer : Int -> Int -> Bool
testCounterServer start guess =
  runPure (
    do server <- Counter.mkServer start
       inc server 1
       inc server 2
       inc server 3
       eq <- (equal server guess)
       pure eq)

main : IO ()
main = printLn (testCounterServer 0 6)
