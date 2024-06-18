module Main where

import Stacky

main :: IO ()
main =
    do
        putStrLn ""
        putStrLn "STACKY v0.1"
        putStrLn ""
        putStrLn "Copyright (c) 2024 Bengt Johansson -- All rights reserved"
        putStrLn "See LICENCE file for more information."
        putStrLn ""
        repl
