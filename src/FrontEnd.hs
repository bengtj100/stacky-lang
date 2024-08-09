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

module FrontEnd(
              parseFile,
              parseLine
             ) where

import CoreTypes
import Lexer(remLongCmt)
import Parser
import Transforms

-------------------------------------------------------------------------------------------------------

doParse :: Env -> String -> String -> Result [Value]
doParse env fname inp =
    do res <- parse env fname inp
       return $ transform env res

parseFile :: Env -> String -> String -> Result [Value]
parseFile env fname inp = doParse env fname (remLongCmt inp)

parseLine :: Env -> String -> Result [Value]
parseLine env inp = doParse env "" inp
