module TestProcesses where

import Control.Monad.IO.Class
import Processes

bounce :: Process (Pid (Pid Integer,Integer))
bounce = spawn $ receive $ clause $ \(pid,a) -> (send pid a :: Process Integer)

printer :: Process (Pid Integer)
printer = spawn $ receive $ clause $ \n -> liftIO . putStrLn . show $ (n::Integer)