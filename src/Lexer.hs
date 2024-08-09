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

module Lexer(
             TokType(..),
             PosTok,
             mylexer,
             remLongCmt
            ) where

import ParseLib
import Data.Char

import Position

specialOps :: [String]
specialOps = ["[", "]", "\'", "^"]

-------------------------------------------------------------------------------------------------------

data TokType = ERROR | DELETE | Str | NumInt | NumFloat | Ident | Op deriving (Show, Eq, Ord)

type Token = (TokType, String)

mkToken :: TokType -> String -> Token
mkToken t str = (t, str)

-------------------------------------------------------------------------------------------------------

type LexError = String

-------------------------------------------------------------------------------------------------------

type PosTok = (Position, TokType, String)

posify :: String -> [Token] -> [PosTok]
posify fName = pfy (initPos fName)
               where pfy currPos ((t, str) : ts) = (currPos, t, str) : pfy (nextPosStr currPos str) ts
                     pfy _       []              = []

-------------------------------------------------------------------------------------------------------

digits     =                                 takeSome isDigit

fraction   =      ok (:)                     `ap` symbol '.' `ap` digits

expon      =      ok (\e s d -> [e]++s++d)   `ap` symbols "eE" `ap` cond (symbols "+-") `ap` digits

integer    =      ok (++)                    `ap` cond (symbol '-') `ap` digits

float      =      ok (\i f e -> i++f++e)     `ap` integer `ap` fraction `ap` optP [] expon
             <||> ok (++)                    `ap` integer `ap` expon

ident ops  =      ok (:)                     `ap` satisfy isAlpha `ap` takeMany isAlphaNum
             <||>                            matchSet ops

operation  =                                 matchSet specialOps

string     =      ok (\s -> "\""++s++"\"")   `chk` symbol '"'
                                             `ap`  escChars
                                             `chk` cut (report strRep (symbol '"'))

escChar    =      ok (\x y -> x:[y])         `ap` symbol '\\' `ap` satisfy (\_->True)
             <||> ok (:[])                   `ap` satisfy (/='"')

escChars   =      ok concat                  `ap` many escChar

whitespace =                                 takeSome isSpace

comment    =      ok (:)                     `ap` symbol '`' `ap` takeMany (/='\n')

strRep :: Reporter Char LexError
strRep _ =  "Missing end quote ('\"') in string"

-------------------------------------------------------------------------------------------------------

number  =      ok (mkToken NumFloat) `ap` float
          <||> ok (mkToken NumInt)   `ap` integer

deletable =    ok (mkToken DELETE)   `ap` (comment  <||> whitespace)

lexemes :: [String] -> Tokenizer
lexemes ops =                            deletable
              <||>                       number
              <||> ok (mkToken Ident)    `ap` ident ops
              <||> ok (mkToken Str)      `ap` string
              <||> ok (mkToken Op)       `ap` operation


-------------------------------------------------------------------------------------------------------

type Tokenizer = Parser Char LexError Token

tokenize :: Tokenizer -> String -> (Token, String)
tokenize _     ""  = (mkToken DELETE "", "")
tokenize tknzr str = case runP tknzr lexErr str of
                         Right x -> x
                         Left  e -> ((ERROR, e), "")

lexErr :: Reporter Char LexError
lexErr []    = "Premature EOF in lexical analysys"
lexErr (c:_) = "Illegal character: " ++ show c

runT :: Tokenizer -> String -> [Token]
runT tk str = case tokenize tk str of
                   (t, "")   -> [t]
                   (t, rest) -> t : runT tk rest


remDEL :: [PosTok] -> [PosTok]
remDEL = filter (\(_, t, _) -> t /= DELETE)

mylexer :: [String] -> String -> String -> [PosTok]
mylexer ops fname = remDEL . posify fname . runT (lexemes ops)

-------------------------------------------------------------------------------------------------------

remLongCmt :: String -> String
remLongCmt = isCmt

isCmt :: String -> String
isCmt ('`' :  '`' :  '`' :  str) = "   " ++ notCmt str
isCmt (c : str) | isSpace c         = c   : isCmt str
                | otherwise         = ' ' : isCmt str
isCmt ""                            = ""


notCmt :: String -> String
notCmt ('`' :  '`' :  '`' :  str) = "   " ++ isCmt str
notCmt (c : str)                     = c : notCmt str
notCmt ""                            = ""
