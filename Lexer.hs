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

module Lexer (
              lexer
             ) where

import Data.Char

import CoreTypes
import BuiltIns
    
-- ====================================================================================================

lexer :: String -> Result ([Value], String)
lexer = lx . dropWhite
        where lx "" =
                  Right ([], "")
              lx str =           
                  do (t, next) <- token str
                     (ts, rest) <- lexer next
                     Right $ (t:ts, rest)

token :: String -> Result (Value, String)
token str@('-' : c :_) | isDigit c = intToken str
token str@(c : _)      | isDigit c = intToken str
token ('"' : str)                  = strToken str
token str                          = atomToken str

strToken :: String -> Result(Value, String)
strToken str = case rest of
                   ""       -> Left $ "Unterminated string constant: '" ++ val ++ "'"
                   _ : rest' -> Right (ValString val, rest')
               where (val, rest) = tok str
                     tok ""                   = ("", "")
                     tok rest'@('"' : _)      = ("", rest')
                     tok ('\\' : '"' : str')  = let (val', rest') = tok str'
                                                in ('"' : val', rest')
                     tok (c : str')           = let (val', rest') = tok str'
                                                in (c: val', rest')
intToken :: String -> Result (Value, String)
intToken str = Right (ValInt x, rest) where (x, rest) = head $ reads str

atomToken :: String -> Result (Value, String)
atomToken (c:d:rest) | [c,d] `elem` ops2 = Right (ValAtom [c,d], rest)
atomToken (c:rest)   | c     `elem` ops1 = Right (ValAtom [c],   rest)
atomToken (c:str)    | isAlpha c         = let (cs, rest) = span (\x -> isAlphaNum x || x=='_') str
                                           in Right (ValAtom (c:cs), rest)
atomToken (c:_)                          = Left ("Unknown character: '" ++ [c] ++ "'")
atomToken ""                             = Left "Out of input data"
                                                              
ops1 :: [Char]
ops1 = ['\'', '[', ']'] ++ [ c | ([c], _) <- builtIns, not $ isAlphaNum c ]

ops2 :: [String]
ops2 = [ [c,d] | ([c,d], _) <- builtIns, not $ isAlphaNum c ]

dropWhite :: String -> String
dropWhite = dropWhile isSpace


