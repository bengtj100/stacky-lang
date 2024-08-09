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

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Parser(
              parse
             ) where

import Data.Char

import CoreTypes
import Position
import ParseLib
import Lexer

-------------------------------------------------------------------------------------------------------

pType :: TokType -> Parser PosTok Error PosTok
pType expType = satisfy (\(_, actType, _) -> expType == actType)

pMatch :: TokType -> String -> Parser PosTok Error PosTok
pMatch expType expStr = satisfy (\(_, actType, actStr) -> expType == actType && expStr == actStr)



-------------------------------------------------------------------------------------------------------

val :: (String -> b) -> (Position -> b -> Value) -> PosTok -> Value
val r vc (p, _, s) = vc p (r s)

pInt       = ok (val read ValInt)               `ap` pType NumInt

pFloat     = ok (val read ValFloat)             `ap` pType NumFloat

pStr       = ok (val read ValString)            `ap` pType Str

pAtom      = ok (val id ValAtom)                `ap` pType Ident

pInhibitor = ok (val id ValAtom)                `ap` (pMatch Op "'" <||> pMatch Op "^")

pList      = ok (\(p, _, _) xs -> ValList p xs) `ap` pMatch Op "[" `ap` pCmds `chk` pMatch Op "]"

pCmd       = anyOf [pList, pInt, pFloat, pStr, pAtom, pInhibitor]

pCmds      = many pCmd

pLang      = ok id `ap` pCmds `chk` atEOF
        
-------------------------------------------------------------------------------------------------------

runParser :: Env -> Parser PosTok Error a -> String -> String -> Either Error (a, [PosTok])
runParser bis p fname inp =
    let
        ops = [ op | bi <- bis
                   , let op = fst bi
                   , not (isAlphaNum (head op))]
    in
        runP p reporter $ mylexer ops fname inp

reporter :: Reporter PosTok Error
reporter []              = (eofPos, "Premature EOF")
reporter ((p, _, _) : _) = (p,      "Syntax Error")

parse :: Env -> String -> String -> Result [Value]
parse bis fname str = do (res, _) <- runParser bis pLang fname str
                         return res
