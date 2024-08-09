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

repC :: String -> Reporter PosTok Error
repC _   []              = (eofPos, "Premature EOF 1")
repC msg ((p, _, _) : _) = (p,      msg)

lexErrRep :: Reporter PosTok Error
lexErrRep []                = (eofPos, "Premature EOF 2")
lexErrRep ((p, _, msg) : _) = (p,      msg)

listRep :: Reporter PosTok Error
listRep xs = case xs of
             []              -> (eofPos, msg)
             ((p, _, _) : _) -> (p,      msg)
             where msg = "Missing end bracket (']') in list"
          
-------------------------------------------------------------------------------------------------------

pType :: TokType -> Parser PosTok Error PosTok
pType expType = satisfy (\(_, actType, _) -> expType == actType)

pMatch :: TokType -> String -> Parser PosTok Error PosTok
pMatch expType expStr = satisfy (\(_, actType, actStr) -> expType == actType && expStr == actStr)

pEOF :: Parser PosTok Error ()
pEOF = report (repC "Garbage after program") atEOF

pLexErr :: Parser PosTok Error a
pLexErr = expErr (pType ERROR) $ cut (report lexErrRep failure)

-------------------------------------------------------------------------------------------------------

val :: (String -> b) -> (Position -> b -> Value) -> PosTok -> Value
val r vc (p, _, s) = vc p (r s)

mkList :: Parser PosTok Error (PosTok -> [Value] -> Value)

mkList = ok (\(p, _, _) xs -> ValList p xs)
         
pInt       = ok (val read ValInt)    `ap` pType NumInt

pFloat     = ok (val read ValFloat)  `ap` pType NumFloat

pStr       = ok (val read ValString) `ap` pType Str

pAtom      = ok (val id ValAtom)     `ap` pType Ident

pInhibitor = ok (val id ValAtom)     `ap` (pMatch Op "'" <||> pMatch Op "^")

pList      = mkList                  `ap`  pMatch Op "["
                                     `ap`  pCmds
                                     `chk` cut (report listRep (pMatch Op "]"))

pCmd       = anyOf [pList, pInt, pFloat, pStr, pAtom, pInhibitor, pLexErr]

pCmds      = many pCmd

pLang      = ok id `ap` many pCmd `chk` pEOF
             
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
