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
-- This module implements a parser for the Stacky language
--
-------------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Parser(
              parse
             ) where

-- System modules
import Data.Char(isAlphaNum)
import Data.List(sortBy)

-- Base modules
import CoreTypes(Env, Error, Result, Value(..))
import Position(Position(..), eofPos)

-- Local modules
import ParseLib(Parser,   runP,
                          ok, failure, satisfy, atEOF, expErr, cut,
                          ap, chk, (<||>),
                          anyOf, many,
                Reporter, report)
    
import Lexer(PosTok, TokType(..), mylexer)

-------------------------------------------------------------------------------------------------------
--  Error reporters
-------------------------------------------------------------------------------------------------------

--
-- Used to report errors from the lexer. It just propagates the
-- position and message from lexer ERROR token.
--
lexErrRep :: Reporter PosTok Error
lexErrRep []                = (eofPos, "Premature EOF 2")
lexErrRep ((p, _, msg) : _) = (p,      msg)

--
-- Report a missing end of list bracket.
--
listRep :: Reporter PosTok Error
listRep xs = case xs of
             []              -> (eofPos, msg)
             ((p, _, _) : _) -> (p,      msg)
             where msg = "Missing end bracket (']') in list"

--
-- This is the default reporter used throughout the parser.
--
defaultRep :: Reporter PosTok Error
defaultRep = repC "Syntax Error"

--
-- Simple constant helper reporter that injects the current position
-- into the report.
--
repC :: String -> Reporter PosTok Error
repC _   []              = (eofPos, "Premature EOF")
repC msg ((p, _, _) : _) = (p,      msg)
          
-------------------------------------------------------------------------------------------------------
--  Primitive parsers
-------------------------------------------------------------------------------------------------------

--
-- Succeeds if the current token is of the given type.
--
pType :: TokType -> Parser PosTok Error PosTok
pType expType = satisfy (\(_, actType, _) -> expType == actType)

--
-- Succeeds if the current token is of the given type and the symbol
-- matches.
--
pMatch :: TokType -> String -> Parser PosTok Error PosTok
pMatch expType expStr = satisfy (\(_, actType, actStr) -> expType == actType && expStr == actStr)

--
-- Report garbage input after the program
--
pEOF :: Parser PosTok Error ()
pEOF = report (repC "Garbage after program") atEOF

--
-- Propagate lexical errors through the parser
--
pLexErr :: Parser PosTok Error a
pLexErr = expErr (pType ERROR) $ cut (report lexErrRep failure)

-------------------------------------------------------------------------------------------------------
--  Main parser
-------------------------------------------------------------------------------------------------------

mkList :: Parser PosTok Error (PosTok -> [Value] -> Value)
mkList = ok (\(p, _, _) xs -> ValList p xs)

mkPos :: Parser PosTok Error (PosTok -> Value)
mkPos = ok (\(p, _, _) -> ValList p [ValString p $ fileName p,
                                     ValInt p $ toInteger $ (1 + linePos p),
                                     ValInt p $ toInteger $ (1 + charPos p)])
        
pPos       = mkPos                   `ap` pMatch Ident "__POS__"

pInt       = ok (val read ValInt)    `ap` pType NumInt

pFloat     = ok (val read ValFloat)  `ap` pType NumFloat

pStr       = ok (val read ValString) `ap` pType Str

pAtom      = ok (val id ValAtom)     `ap` pType Ident

pInhibitor = ok (val id ValAtom)     `ap` (pMatch Op "'" <||> pMatch Op "^")

pList      = mkList                  `ap`  pMatch Op "["
                                     `ap`  pCmds
                                     `chk` cut (report listRep (pMatch Op "]"))

pCmd       = anyOf [pPos, pInt, pFloat, pStr, pAtom, pInhibitor, pList, pLexErr]

pCmds      = many pCmd

pLang      = ok id `ap` many pCmd `chk` pEOF
             
-------------------------------------------------------------------------------------------------------
--  Public API
-------------------------------------------------------------------------------------------------------

--
-- Main parser function
--
-- It takes the following inputs:
--   - bis   :: Env    : Used to generate a list of non alphanum operations.
--   - fname :: String : The name of the file the program comes from.
--   - inp   :: String : The input string to be parsed.
--
parse :: Env -> String -> String -> Result [Value]
parse bis fname str = do (res, _) <- runParser bis pLang fname str
                         return res

--
-- Helper function for `parse`
--
runParser :: Env -> Parser PosTok Error a -> String -> String -> Either Error (a, [PosTok])
runParser bis p fname inp =
    let
        ops = sortByLen [ op | bi <- bis
                             , let (op,_,_) = bi
                             , not (isAlphaNum (head op))]
    in
        runP p defaultRep $ mylexer ops fname inp

--
-- Sort a list of strings on their lenghts. Longest first.
-- This way "++" will be parsed as one token and not two "+".
--
sortByLen :: [String] -> [String]
sortByLen = sortBy longestFirst
            where longestFirst s1 s2 | l1 > l2   = LT
                                     | l1 < l2   = GT
                                     | otherwise = EQ
                                     where l1 = length s1
                                           l2 = length s2

-------------------------------------------------------------------------------------------------------
--  General helper functions
-------------------------------------------------------------------------------------------------------

--
-- Build a Value from a token string
--
val :: (String -> b) -> (Position -> b -> Value) -> PosTok -> Value
val r vc (p, _, s) = vc p (r s)
