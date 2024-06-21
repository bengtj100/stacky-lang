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

module Repl (repl) where

import InputOutput

import CoreTypes
import Interpreter
import BuiltIns

-- ====================================================================================================

repl :: IO ()
repl = r (initCxt builtIns)
       where r cxt =
                 do -- putStrLn "STACKY V1.0 (c) Bengt Johansson"
                    line         <- getLines "> "
                    let parseRes =  parseLine line
                    result       <- ifOk parseRes $ \cmds -> interpreter cxt cmds
                    nextCxt      <- either (\err    -> do { printError err; return cxt; })
                                           (\newCxt -> do printStack newCxt
                                                          return newCxt)
                                           result
                    r nextCxt
