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
-- This module implements a lexer for the Stacky language.
--
-- It is based on the same parser library as the parser.
--
-- It works in several steps:
--   1) A basic recognizer is run to generate tokens for all kinds of lexemes
--   2) The lexemes are analyzed and positions are computed and added to the tokens.
--   3) Some lexemes, like whitespace and comments are removed from
--      the resulting symbol list
--
-------------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Lexer(
             TokType(..),
             PosTok,
             mylexer,
             remLongCmt
            ) where

-- System modules
import Data.Char

-- Base modules
import Position

-- Local modules
import ParseLib

-------------------------------------------------------------------------------------------------------
--  Special operations that are not builtins.
-------------------------------------------------------------------------------------------------------

specialOps :: [String]
specialOps = ["[", "]", "\'", "^"]

-------------------------------------------------------------------------------------------------------
--  The token type we use for the output token list.
-------------------------------------------------------------------------------------------------------

--
-- Type of token
--
data TokType = ERROR | DELETE | Str | NumInt | NumFloat | Ident | Op deriving (Show, Eq, Ord)

--
-- A token is a type and a symbol as a string.
--
type Token = (TokType, String)

--
-- Helper function to create a token.
--
mkToken :: TokType -> String -> Token
mkToken t str = (t, str)

-------------------------------------------------------------------------------------------------------
-- The lexer produces error messages that are strings
-------------------------------------------------------------------------------------------------------

type LexError = String

-------------------------------------------------------------------------------------------------------
--  Definitions of the basic lexemes.
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
--  Definition of the lexeme recognizer
-------------------------------------------------------------------------------------------------------

--
-- Numbers are kept apart to ensure that floats are tested before
-- integer.
--
number  =      ok (mkToken NumFloat) `ap` float
          <||> ok (mkToken NumInt)   `ap` integer

--
-- These are tokens that can be deleted once positions are added
--
deletable =    ok (mkToken DELETE)   `ap` (comment  <||> whitespace)

--
-- This is the main recognizer. It recognizes one token at a time.
--
lexemes :: [String] -> Tokenizer
lexemes ops =                            deletable
              <||>                       number
              <||> ok (mkToken Ident)    `ap` ident ops
              <||> ok (mkToken Str)      `ap` string
              <||> ok (mkToken Op)       `ap` operation

-------------------------------------------------------------------------------------------------------
--  The tokenizer
-------------------------------------------------------------------------------------------------------

type Tokenizer = Parser Char LexError Token

--
-- Helper function that runs the recognizer on one token. It returns
-- the token and the rest of the unprocessed input.
--
-- In case of an error, it reports an empty unprocessed list to stop
-- the tokenization process.
--
tokenize :: Tokenizer -> String -> (Token, String)
tokenize _     ""  = (mkToken DELETE "", "")
tokenize tknzr str = case runP tknzr lexErr str of
                         Right x -> x
                         Left  e -> ((ERROR, e), "")

--
-- Default reporter for lexical errors.
--
lexErr :: Reporter Char LexError
lexErr []    = "Premature EOF in lexical analysis"
lexErr (c:_) = "Illegal character: " ++ show c

--
-- Main tokenizer function.
--
-- It lazily tokenizes the entire input list.
--
runT :: Tokenizer -> String -> [Token]
runT tk str = case tokenize tk str of
                   (t, "")   -> [t]
                   (t, rest) -> t : runT tk rest


-------------------------------------------------------------------------------------------------------
--  Adding positions to tokens and removing garbage
-------------------------------------------------------------------------------------------------------

--
-- This is the exported token type.
--
type PosTok = (Position, TokType, String)

--
-- The main function of the public API.
--
mylexer :: [String] -> String -> String -> [PosTok]
mylexer ops fname = remDEL . posify fname . runT (lexemes ops)

--
-- Helper function that adds positions to the tokens.
--
posify :: String -> [Token] -> [PosTok]
posify fName = pfy (initPos fName)
               where pfy currPos ((t, str) : ts) = (currPos, t, str) : pfy (nextPosStr currPos str) ts
                     pfy _       []              = []

--
-- Helper function that removes deletable tokens.
remDEL :: [PosTok] -> [PosTok]
remDEL = filter (\(_, t, _) -> t /= DELETE)

-------------------------------------------------------------------------------------------------------
-- Dealing with long comments '``` ... ```'
-------------------------------------------------------------------------------------------------------

--
-- This is an exported function that removes long comments from the
-- input. Since it is only needed when reading files of Stacky code
-- and not in the REPL of the interpreted, it is used outside the
-- lexer wherever is appropriate.
--
-- It is implemented as a simple state machine encoded in the
-- functions `isCmt` and `notCmt`. Initially we assume that we are in
-- a long comment, since Stacky uses literate programming.
--
-- All non whitespace characters are replaced with space ' ' to ensure
-- proper computation of positions in the lexer.
--
remLongCmt :: String -> String
remLongCmt = isCmt

--
-- The state of being in a comment.
--
isCmt :: String -> String
isCmt ('`' :  '`' :  '`' :  str) = "   " ++ notCmt str
isCmt (c : str) | isSpace c      = c   : isCmt str
                | otherwise      = ' ' : isCmt str
isCmt ""                         = ""

--
-- The state of being outside a comment.
--
notCmt :: String -> String
notCmt ('`' :  '`' :  '`' :  str) = "   " ++ isCmt str
notCmt (c : str)                  = c : notCmt str
notCmt ""                         = ""
