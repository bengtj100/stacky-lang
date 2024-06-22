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
              OpsData,
              lexer,
              lexerDirect
             ) where

import Data.Char
import Data.List.Split (splitOn)

import CoreTypes

type OpsData = ([Char], [String])
    
-- ====================================================================================================

lexer :: OpsData -> String -> Result ([Value], String)
lexer ops = lexerDirect ops . removeComments

lexerDirect ::  OpsData -> String -> Result ([Value], String)
lexerDirect ops =
    lx . dropWhite
    where lx ""  = Right ([], "")
          lx str = do (t, next) <- token ops str
                      (ts, rest) <- lexerDirect ops next
                      Right $ (t:ts, rest)

token :: OpsData -> String -> Result (Value, String)
token _   str@('-' : c :_) | isDigit c = intToken str
token _   str@(c : _)      | isDigit c = intToken str
token _   ('"' : str)                  = strToken str
token ops str                          = atomToken ops str

strToken :: String -> Result(Value, String)
strToken str = case rest of
                   ""       -> Left $ "Unterminated string constant: '" ++ val ++ "'"
                   _ : rest' -> Right (ValString val, rest')
               where (val, rest) = tok str
                     tok ""                    = ("", "")
                     tok rest'@('"' : _)       = ("", rest')
                     tok ('\\' : '"' : str')   = let (val', rest') = tok str'
                                                 in ('"' : val', rest')
                     tok ('\\' : 'n' : str')   = let (val', rest') = tok str'
                                                 in ('\n' : val', rest')
                     tok ('\\' : 'r' : str')   = let (val', rest') = tok str'
                                                 in ('\n' : val', rest')
                     tok ('\\' : 't' : str')   = let (val', rest') = tok str'
                                                 in ('\n' : val', rest')
                     tok ('\\' : '\\' : str')  = let (val', rest') = tok str'
                                                 in ('\n' : val', rest')
                     tok (c : str')            = let (val', rest') = tok str'
                                                 in (c: val', rest')
intToken :: String -> Result (Value, String)
intToken str = Right (ValInt x, rest) where (x, rest) = head $ reads str

atomToken :: OpsData -> String -> Result (Value, String)
atomToken (_, ops2) (c:d:rest) | [c,d] `elem` ops2 = Right (ValAtom [c,d], rest)
atomToken (ops1, _) (c:rest)   | c     `elem` ops1 = Right (ValAtom [c], rest)
atomToken _         (c:str)    | isAlpha c         = let (cs, rest) = spanName str
                                                     in Right (ValAtom (c:cs), rest)
atomToken _         (c:_)                          = Left ("Unknown character: '" ++ [c] ++ "'")
atomToken _         ""                             = Left "Out of input data"

spanName :: String -> (String, String)
spanName = span (\x -> isAlphaNum x || x=='_')

dropWhite :: String -> String
dropWhite = dropWhile isSpace


-- ====================================================================================================

removeComments :: String -> String
removeComments = removeShortComments . removeLongComments

removeLongComments :: String -> String
removeLongComments = unlines . remCmts . splitOn "```"

remCmts :: [String] -> [String]
remCmts (_ : nonCmt : xs) = nonCmt : remCmts xs
remCmts _                 = []

removeShortComments :: String -> String
removeShortComments ""           = ""
removeShortComments ('`' : rest) = removeShortComments rest'
                                   where rest' = dropWhile ((/=) '\n') rest
removeShortComments (c : str)    = c : removeShortComments str
