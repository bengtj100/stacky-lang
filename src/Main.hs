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
import CommandLine


-- ====================================================================================================

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

-- ----------------------------------------------------------------------------------------------------

mainBody :: CmdRes -> (Cxt -> IO ()) -> IO ()
mainBody opts handler =
    do
        res <- runPrelude opts
        case res of
            Nothing ->
                exitWith (ExitFailure 1)
            Just cxt ->
                handler cxt

-- ====================================================================================================

runRepl :: Cxt -> IO ()
runRepl cxt = repl cxt `catch` handleError
        
-- ----------------------------------------------------------------------------------------------------

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
