module Stacky (repl) where
                     
import System.IO

import CoreTypes
import Interpreter
import Parser
import BuiltIns

-- ====================================================================================================

repl :: IO ()
repl = r (initCxt builtIns)
       where r cxt =
                 do -- putStrLn "STACKY V1.0 (c) Bengt Johansson"
                    putStr "> "
                    hFlush stdout
                    line         <- getLine
                    let parseRes =  parser line
                    result       <- ifOk parseRes $ \cmds -> interpreter cxt cmds
                    nextCxt      <- either (\err    -> do { printError err; return cxt; })
                                           (\newCxt -> do printStack newCxt
                                                          return newCxt)
                                           result
                    r nextCxt
                               
