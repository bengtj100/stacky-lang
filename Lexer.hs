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

type Lexer = Position -> String -> Result ([Value], String, Position)
    
-- ====================================================================================================

lexer :: OpsData -> Lexer
lexer ops pos = lexerDirect ops pos . removeComments pos

lexerDirect ::  OpsData -> Lexer
lexerDirect ops pos str =
    lx newPos newStr
    where (newStr, newPos) = dropWhite pos str
          lx pos0 ""   = Right ([], "", pos0)
          lx pos0 str0 = do (t, next, pos1) <- token ops pos0 str0
                            (ts, rest, pos2) <- lexerDirect ops pos1 next
                            Right $ (t:ts, rest, pos2)

token :: OpsData -> Position -> String -> Result(Value, String, Position)
token _   pos str@('-' : c :_) | isDigit c = intToken pos str
token _   pos str@(c : _)      | isDigit c = intToken pos str
token _   pos ('"' : str)                  = strToken pos str
token _   pos ('@':'@':'@':str)            = posToken pos str                 
token ops pos str                          = atomToken ops pos str

posToken :: Position -> String -> Result(Value, String, Position)
posToken _ str =
    Right (ValNoop, rest, read posStr)
    where
        (posStr, rest) = span (/= '\n') str
        
 
strToken :: Position -> String -> Result(Value, String, Position)
strToken pos str =
    case rest of
        ""        -> newErrPos pos $ "Unterminated string constant: '" ++ val ++ "'"
        _ : rest' -> Right (ValString pos val, rest', pos1)
    where (val, rest, pos1)              = tok pos str
          tok pos0 ""                    = ("", "", pos0)
          tok pos0 rest'@('"' : _)       = ("", rest', incPosChar pos0 1)
          tok pos0 ('\\' : '"' : str')   = let (val', rest', pos2) = tok (incPosChar pos0 2) str'
                                           in ('"' : val', rest', pos2)
          tok pos0 ('\\' : 'n' : str')   = let (val', rest', pos2) = tok (incPosChar pos0 2) str'
                                           in ('\n' : val', rest', pos2)
          tok pos0 ('\\' : 'r' : str')   = let (val', rest', pos2) = tok (incPosChar pos0 2) str'
                                           in ('\n' : val', rest', pos2)
          tok pos0 ('\\' : 't' : str')   = let (val', rest', pos2) = tok (incPosChar pos0 2) str'
                                           in ('\n' : val', rest', pos2)
          tok pos0 ('\\' : '\\' : str')  = let (val', rest', pos2) = tok (incPosChar pos0 2) str'
                                           in ('\n' : val', rest', pos2)
          tok pos0 (c : str')            = let (val', rest', pos2) = tok (nextPos pos0 c) str'
                                           in (c    : val', rest', pos2)

intToken :: Position -> String -> Result(Value, String, Position)
intToken pos str =
    Right (ValInt pos x, rest, pos1)
    where (x, rest) = head $ reads str
          pos1      = incPosChar pos $ length $ show x
                      
atomToken :: OpsData -> Position -> String -> Result(Value, String, Position)
atomToken (_, ops2) pos (c:d:rest)
    | [c,d] `elem` ops2 =
        Right (ValAtom pos [c,d], rest, nextPosStr pos [c,d])
atomToken (ops1, _) pos (c:rest)
    | c `elem` ops1 =
        Right (ValAtom pos [c], rest, nextPos pos c)
atomToken _ pos (c:str)
    | isAlpha c =
        let (cs, rest) = spanName str
        in Right (ValAtom pos (c:cs), rest, nextPosStr pos (c:cs))
atomToken _ pos (c:_) =
    newErrPos pos ("Unknown character: '" ++ [c] ++ "'")
atomToken _ pos "" =
    newErrPos pos "Out of input data"

spanName :: String -> (String, String)
spanName = span (\x -> isAlphaNum x || x=='_')

dropWhite :: Position -> String -> (String, Position)
dropWhite pos str = (rest, nextPosStr pos ws)
                    where (ws, rest) = span isSpace str


-- ====================================================================================================

removeComments :: Position -> String -> String
removeComments pos = removeShortComments . removeLongComments pos

removeLongComments :: Position -> String -> String
removeLongComments pos = unlines . remCmts pos . splitOn "```"

remCmts :: Position -> [String] -> [String]
remCmts pos (cmt : nonCmt : xs) = marker : nonCmt : remCmts nonCmtPos xs
                                  where cmtPos = nextPosStr pos cmt
                                        nonCmtPos = nextPosStr cmtPos nonCmt
                                        marker    = "@@@" ++ show cmtPos
remCmts _ _ = []

removeShortComments :: String -> String
removeShortComments ""           = ""
removeShortComments ('`' : rest) = removeShortComments rest'
                                   where rest' = dropWhile ('\n' /=) rest
removeShortComments (c : str)    = c : removeShortComments str
