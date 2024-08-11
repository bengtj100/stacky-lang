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

-- Base modules
import CoreTypes(Env, Result, Value)

-- Local modules
import Lexer(remLongCmt)
import Parser(parse)
import Transforms(transform)

-------------------------------------------------------------------------------------------------------
--  Main API functions
-------------------------------------------------------------------------------------------------------

--
-- Use this when loading an entire Stacky module from file.
-- It will eliminate long comments.
--
parseFile :: Env -> String -> String -> Result [Value]
parseFile env fname inp = doParse env fname (remLongCmt inp)

--
-- Use this when entering commands in the REPL or in operations like
-- `eval`.
--
parseLine :: Env -> String -> Result [Value]
parseLine env inp = doParse env "" inp

-------------------------------------------------------------------------------------------------------
--  Helper functions
-------------------------------------------------------------------------------------------------------

--
-- Call the parser and perform all transforms.
-- Common functionality to the public functions.
--
doParse :: Env -> String -> String -> Result [Value]
doParse env fname inp =
    do res <- parse env fname inp
       return $ transform env res

-------------------------------------------------------------------------------------------------------
--  That's all folks!
-------------------------------------------------------------------------------------------------------
