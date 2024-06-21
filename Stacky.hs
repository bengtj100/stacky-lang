-- ====================================================================================================
--
-- Copyright (c) 2024 Bengt Johansson <bengtj100 at gmail dot com>.
-- All rights reserved.
--
-- This software is part of the stacky project and its use is
-- regulated by the conditions stipulated in the file named 'LICENCE',
-- located in the top directory of said project.
--
-- ====================================================================================================

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
                               
