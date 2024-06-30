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
import Interpreter
import BuiltIns

-- ====================================================================================================

runPrelude :: [Value] -> String -> IO (Maybe Cxt)
runPrelude cmds pFile=
    do let cxt =  initCxt builtIns
       prl     <- makePrelude pFile cmds
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
            
makePrelude :: String -> [Value] -> IO [Value]
makePrelude pFile cmds =
    do path <- if pFile == ""
               then do exePath <- getExecutableDir
                       return $ exePath ++ "/../lib/Prelude.sy"
               else return pFile
       return $ [ValString noPos path, ValAtom noPos "import"] ++ cmds


getExecutableDir :: IO String
getExecutableDir =
    do path <- getExecutablePath
       return $ dirname path

dirname :: String -> String
dirname path =
    intercalate "/" $ init $ split "/" path
