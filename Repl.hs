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

module Repl (
             runPrelude,
             repl
            ) where

import InputOutput

import CoreTypes
import Interpreter
import BuiltIns

-- ====================================================================================================

runPrelude :: [Value] -> IO (Maybe Cxt)
runPrelude cmds =
    do let cxt = initCxt builtIns
       result <- interpreter cxt cmds
       case result of
           Left  err  -> do printError err
                            return $ Nothing
           Right cxt' -> return $ Just cxt'
                   
repl :: Cxt -> IO ()
repl cxt = do printStack cxt
              loop cxt

loop :: Cxt -> IO ()
loop cxt =
    do line         <- getLines "> "
       let parseRes =  parseLine line
       result       <- ifOk parseRes $ \cmds -> interpreter cxt cmds
       either (handleError cxt)
              handleSuccess
              result

handleSuccess :: Cxt -> IO ()
handleSuccess cxt =
    do printStack cxt
       loop cxt

handleError :: Cxt -> Error -> IO ()
handleError cxt err =
    do printError err
       loop cxt
            
