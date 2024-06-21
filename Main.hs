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

import Stacky

-- ====================================================================================================

main :: IO ()
main =
    do
        putStrLn ""
        putStrLn "STACKY v0.1"
        putStrLn ""
        putStrLn "Copyright (c) 2024 Bengt Johansson -- All rights reserved"
        putStrLn ""
        repl
