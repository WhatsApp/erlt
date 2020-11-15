module Main
import Counter

testCounterServer : Int -> Int -> IO ()
testCounterServer start guess =
    do server <- Counter.spawn start
       inc server 1
       inc server 2
       inc server 3
       eq <- equal server guess
       printLn eq

main : IO ()
main = testCounterServer 0 6
