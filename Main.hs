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

{-# LANGUAGE QuasiQuotes #-}

module Main where

import System.IO.Error
import System.Exit
import System.Environment
import Control.Exception

import Text.RawString.QQ

import CoreTypes
import Repl
import Version
import CommandLine


-- ====================================================================================================

main :: IO ()
main =
    do args <- getArgs
       let cargs = parseArguments args
       case cargs of
           CmdError errMsg -> printErrorWithProgname (noPos, errMsg)
           CmdVersion      -> printVersion
           CmdUsage        -> printUsage
           CmdArg{interactive = i,
                  prelude     = p} -> runMain i p

runMain :: Bool -> [Value] -> IO ()
runMain int prel =
    do res <- runPrelude prel
       case res of
           Nothing -> exitWith (ExitFailure 1)
           Just cxt ->
               if int
                  then runRepl cxt
                  else exitWith ExitSuccess


printVersion :: IO ()
printVersion =
    do putStrLn $ "STACKY version: " ++ version ++", build: " ++ build
       putStrLn "Copyright (c) 2024 Bengt Johansson -- All rights reserved"
       putStrLn ""
       exitWith ExitSuccess

printUsage :: IO ()
printUsage =
    do pName <- getProgName
       putStrLn $ usageStr pName
       exitWith ExitSuccess

usageStr :: String -> String
usageStr pName =
    [r|
Usage: |] ++ pName ++ [r| ( <option> | <module-name> )*

Where option is one of:

--eval, -e <stacky-code>    Evaluate <stacky-code>
--interactive, -i           Run in interactive mode, i.e., run the REPL after Prelude and
                            all modules are loaded.
--batch, -b                 Run in batch mode, i.e., terminate once all modules are loaded.
--version                   Print the current version and terminate.
--help, -h                  Print this message and then terminate.

The interpreter will load all modules and execute the '--eval' options in the
order they appear on the command line. If in interactive mode, the REPL will
run after all loading is complete and in batch mode, the interpreter will
terminate once all is loaded and executed.

|]
    
-- ====================================================================================================

runRepl :: Cxt -> IO ()
runRepl cxt =
    do printGreeting
       repl cxt`catch` handleError

-- ====================================================================================================

printGreeting :: IO ()
printGreeting =
    do putStrLn ""
       putStrLn $ "STACKY version: " ++ version ++", build: " ++ build
       putStrLn ""
       putStrLn "Copyright (c) 2024 Bengt Johansson -- All rights reserved"
       putStrLn ""
        
-- ====================================================================================================

handleError :: IOError -> IO ()
handleError err =
    if isEOFError err
    then do putStrLn ""
            putStrLn "Leaving stacky interpreter"
            putStrLn "Bye!"
            putStrLn ""
            exitWith ExitSuccess
    else do pname <- getProgName
            putStrLn ""
            putStrLn $ pname ++ ": " ++ show err
            putStrLn "Terminating!"
            exitWith (ExitFailure 1)
