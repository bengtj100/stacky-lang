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
    
import Repl

-- ====================================================================================================

main :: IO ()
main =
    do printGreeting
       repl `catch` handleError

-- ====================================================================================================

printGreeting :: IO ()
printGreeting =
    do putStrLn ""
       putStrLn "STACKY v0.1"
       putStrLn ""
       putStrLn "Copyright (c) 2024 Bengt Johansson -- All rights reserved"
       putStrLn ""
        
-- ====================================================================================================

handleError :: IOError -> IO ()
handleError err =
    if isEOFError err && False
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
