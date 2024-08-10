-------------------------------------------------------------------------------------------------------
--
-- Copyright (c) 2024 Bengt Johansson <bengtj100 at gmail dot com>.
-- All rights reserved.
--
-- This software is part of the stacky project and its use is
-- regulated by the conditions stipulated in the file named 'LICENCE',
-- located in the top directory of said project.
--
-------------------------------------------------------------------------------------------------------
--
-- This module implements the REPL of the Stacky interpreter.
--
-------------------------------------------------------------------------------------------------------

module Repl (
             repl,
             runPrelude             
            ) where

import Data.List
import Data.List.Utils
import System.Environment
    
import InputOutput

import CoreTypes
import Position
import Interpreter
import BuiltIns
import FrontEnd
import CommandLine

-------------------------------------------------------------------------------------------------------
--  repl - The read, eval, print loo of the interpreter
-------------------------------------------------------------------------------------------------------

--
-- `repl` and `loop` runs the main REPL.
--
-- `repl` is the entry-point and sets things up. Once finished it hands over to the loop.
-- `loop` runs the REPL main loop.
repl :: Cxt -> IO ()
repl cxt = do printStack cxt
              loop cxt

--
-- The main REPL loop
--
loop :: Cxt -> IO ()
loop cxt =
    do line         <- getLines "> "
       let parseRes =  parseLine builtIns line
       result       <- ifOk parseRes $ \cmds -> interpreter cxt cmds
       either (handleError cxt)
              handleSuccess
              result

--
-- Handles a successful evaluation result. It prints the modified
-- stack and then hands over to the loop again.
--
handleSuccess :: Cxt -> IO ()
handleSuccess cxt =
    do printStack cxt
       loop cxt

--
-- Handles an error result. It prints the error message and then hands
-- over to the loop again.
--
handleError :: Cxt -> Error -> IO ()
handleError cxt err =
    do printError err
       loop cxt

-------------------------------------------------------------------------------------------------------
--  runPrelude - Create a list of commands to run before running the REPL and run it
-------------------------------------------------------------------------------------------------------

--
-- This is the entry point. It:
--   1) Creates a list of commands to evaluate
--   2) Runs the interpreter on that list
--   3) Hands the updated Context to the caller
--
runPrelude :: CmdRes -> IO (Maybe Cxt)
runPrelude opts =
    do let cxt =  initCxt builtIns
       prl     <- makePrelude opts
       result  <- interpreter cxt prl
       case result of
           Left  err  -> do printError err
                            return $ Nothing
           Right cxt' -> return $ Just cxt'
                   
--
-- Create the prelude commands. It contains commands that:
--   1) It defines `isInteractive` to either true or false
--   2) It imports the Prelude.sy module
--   3) Execute any previously defined otions. (--eval or module names)
--
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

--
-- Create a variable definition
--
setDef :: String -> Value -> [Value]
setDef name val = [val, ValAtom noPos "'", ValAtom noPos name, ValAtom noPos ";"]

--
-- Find the directory the executable is located in
--
-- Similar to BASH: dirname "$0"
--
splitExecutableDir :: IO (String, String)
splitExecutableDir =
    do path <- getExecutablePath
       return $ splitDirname path

--
-- Split a path into the executable file's name and leading path. Much
-- like `dirname` and `basename` in BASH.
--
splitDirname :: String -> (String, String)
splitDirname path = (dirName, exeName)
              where dirName = intercalate "/" $ init parts 
                    exeName = last parts
                    parts   = split "/" path

-------------------------------------------------------------------------------------------------------
--  That's all folks!!
-------------------------------------------------------------------------------------------------------

