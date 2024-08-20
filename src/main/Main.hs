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
-- This module implements the main function of the Stacky
-- interpreter. It parses command-line arguments, loads the prelude
-- and any specified module. If in interactive mode, is also runs the
-- REPL.
-------------------------------------------------------------------------------------------------------

module Main(main) where

-- System modules
import System.Exit(exitWith, ExitCode(..))
import System.Environment(getArgs)

-- Base modules
import Position(noPos)
import CoreTypes( Cxt(..), initCxt
                , printError
                , Value(..)
                )

-- Interpreter modules
import Interpreter(interpreter)
import BuiltIns(builtIns)

-- Local modules
import CommandLine(CmdRes(..), parseArguments)
import LibraryPath(loadLibPath)

-------------------------------------------------------------------------------------------------------
--  The main function
-------------------------------------------------------------------------------------------------------

--
-- Get and parse arguments, then hand over to the main body.
-- If interactive, send the REPL to the main body as a handler
--
main :: IO ()
main =
    do
        args <- getArgs
        opts <- parseArguments args
        res  <- runPrelude opts
        case res of
            Nothing ->
                exitWith (ExitFailure 1)
            Just _ ->
                return ()

-------------------------------------------------------------------------------------------------------
--  Helper functions
-------------------------------------------------------------------------------------------------------

--
--  Create a list of commands to run before running the REPL and run it
--
-- This is the entry point. It:
--   1) Initializes the context w.r.t. load paths
--   2) Creates a list of commands to evaluate
--   3) Runs the interpreter on that list
--   4) Hands the updated Context to the caller
--
runPrelude :: CmdRes -> IO (Maybe Cxt)
runPrelude opts =
    do let cxt0 =  initCxt builtIns
           cxt1 =  loadArgv opts cxt0
       cxt      <- loadLibPath (incPrePaths opts) (incAppPaths opts) cxt1
       prl      <- makePrelude opts
       result   <- interpreter cxt prl
       case result of
           Left  err  -> do printError err
                            return $ Nothing
           Right cxt' -> return $ Just cxt'

-------------------------------------------------------------------------------------------------------

--
-- Move the argv component from the CmdRes to the Cxt.
--
loadArgv :: CmdRes -> Cxt -> Cxt
loadArgv (CmdRes{cmdOpts = opts}) cxt = cxt{argv = opts}
                                   
-------------------------------------------------------------------------------------------------------

--
-- Create the prelude commands. It contains commands that:
--   1) It defines `isInteractive` to either true or false
--   2) It imports the Prelude.sy module
--   3) Execute any previously defined otions. (--eval or module names)
--   4) Import the Repl
--
makePrelude :: CmdRes -> IO [Value]
makePrelude opts =
    let name          = preludeFile opts
        path          = if name == "" then "Prelude" else name
        isInteractive = setDef "isInteractive" $ ValInt noPos (if interactive opts then 1 else 0)
    in return (isInteractive
               ++ [ValString noPos "Greeting", ValAtom noPos "import"]
               ++ [ValString noPos path, ValAtom noPos "import"]
               ++ prelude opts
               ++ [ValString noPos "Repl", ValAtom noPos "import"])
    
-------------------------------------------------------------------------------------------------------

--
-- Create a variable definition
--
setDef :: String -> Value -> [Value]
setDef name val = [val, ValAtom noPos "'", ValAtom noPos name, ValAtom noPos ";"]

-------------------------------------------------------------------------------------------------------
--  That's all folks!!
-------------------------------------------------------------------------------------------------------

