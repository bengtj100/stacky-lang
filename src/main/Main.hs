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
import System.IO.Error(isEOFError)
import System.Exit(exitWith, ExitCode(..))
import System.Environment(getArgs, getProgName)
import Control.Exception(catch)

-- Base modules
import CoreTypes(Cxt)

-- Local modules
import Repl(repl, runPrelude)
import CommandLine(CmdRes(..), parseArguments, printGreeting)

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
        if interactive opts then
            do
                printGreeting
                mainBody opts $ runRepl
        else
            do
                mainBody opts $ \_ -> return ()

-------------------------------------------------------------------------------------------------------
--  Helper functions
-------------------------------------------------------------------------------------------------------

--
-- Main body helper. This is the same no matter whether interactive or
-- not. It just runs the prelude and hands the context over either to
-- the REPL or to a null handler.
--
mainBody :: CmdRes -> (Cxt -> IO ()) -> IO ()
mainBody opts handler =
    do
        res <- runPrelude opts
        case res of
            Nothing ->
                exitWith (ExitFailure 1)
            Just cxt ->
                handler cxt

--
-- Helper functions that catches errors from the REPL so they does not
-- get exposed to the outer layers.
--
runRepl :: Cxt -> IO ()
runRepl cxt = repl cxt `catch` handleError

handleError :: IOError -> IO ()
handleError err =
    if isEOFError err then
        do putStrLn ""
           putStrLn "Leaving stacky interpreter"
           putStrLn "Bye!"
           putStrLn ""
           exitWith ExitSuccess
    else
        do pname <- getProgName
           putStrLn ""
           putStrLn $ pname ++ ": " ++ show err
           putStrLn "Terminating!"
           exitWith (ExitFailure 1)
