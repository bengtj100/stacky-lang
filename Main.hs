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

module Main where

import System.IO.Error
import System.Exit
import System.Environment
import Control.Exception

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
           CmdError errMsg -> printError (noPos, errMsg)
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
    do putStrLn "Usage ..."
       putStrLn ""
       exitWith ExitSuccess

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
