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

import Data.List
import Data.List.Utils
import System.Environment
    
import InputOutput

import CoreTypes
import Position
import Interpreter
import BuiltIns
import CommandLine

-- ====================================================================================================

runPrelude :: CmdRes -> IO (Maybe Cxt)
runPrelude opts =
    do let cxt =  initCxt builtIns
       prl     <- makePrelude opts
       result  <- interpreter cxt prl
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
       let parseRes =  parseLine builtIns line
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


makePrelude :: CmdRes -> IO [Value]
makePrelude opts =
    do
        (exePath,exeName) <- splitExecutableDir
        let path = if (preludeFile opts) == "" then
                       exePath ++ "/../lib/" ++ exeName ++ "/Prelude.sy"
                   else
                       preludeFile opts
        let isInteractive =
                setDef "isInteractive" $ ValInt noPos (if interactive opts then 1 else 0)
        return $ isInteractive ++ [ValString noPos path, ValAtom noPos "import"] ++ prelude opts

setDef :: String -> Value -> [Value]
setDef name val = [val, ValAtom noPos "'", ValAtom noPos name, ValAtom noPos ";"]

splitExecutableDir :: IO (String, String)
splitExecutableDir =
    do path <- getExecutablePath
       return $ splitDirname path

splitDirname :: String -> (String, String)
splitDirname path = (dirName, exeName)
              where dirName = intercalate "/" $ init parts 
                    exeName = last parts
                    parts   = split "/" path
