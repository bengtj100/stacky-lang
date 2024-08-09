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
-- This module implements a simple combinator based parser library.
--
-- It is able to report errors and the position in the file they
-- occurred. It also supports 'cuts', i.e., errors that pass through
-- undisturbed to the top.
--
-------------------------------------------------------------------------------------------------------

module ParseLib where

import Data.List(isPrefixOf)
    
-------------------------------------------------------------------------------------------------------
--  Reporter - A type for error reports
-------------------------------------------------------------------------------------------------------

--
-- The reporter type is used to generate customized error reports. It
-- is given the token list so that it can inspect it and customize the
-- message accordingly.
--
type Reporter symb err = [symb] -> err

--
-- Create a constant reporter, i.e., one that ignores the input list.
--
constR :: e -> Reporter a e
constR e _ = e
             
-------------------------------------------------------------------------------------------------------
--  Cut - Representing cuts
-------------------------------------------------------------------------------------------------------

--
-- Internally, the parser represents cuts as a boolean flag. If it is
-- set (True) then the error is a cut and will not be handled by the
-- normal combinators.
--
type Cut = Bool

--
-- Lift an error to a cuttable error. By default not a cut.
--
regErr :: err -> (Cut, err)
regErr e = (False, e)

--
-- Test if an error is a cut.
--
isCut :: (Cut, err) -> Bool
isCut (c, _) = c

--
-- Unconditinally register a cuttabl error as a cut.
--
mkCut :: (Cut, err) -> (Cut, err)
mkCut (_, e) = (True, e)

-------------------------------------------------------------------------------------------------------
--  Parser - The main parser type
-------------------------------------------------------------------------------------------------------

--
-- A parser is simply a function that takes an error reporter and a
-- list of symbols and either produces a parsed result or an error.
--
type Parser symb err res = (Reporter symb err) -> [symb] -> Either (Cut, err) (res, [symb])
                                     
--
-- Run a parser. Given a parser to parse things with, a default error
-- reporter and a list of symbols this function returns one of two
-- possible outcomes:
--
--     Left err - The parser failed and `err` is a message explaning
--                why.

--     Right (res, rest) - The parser was successful and `res` is the
--                         parsed result.
--                         `rest` is any tokens the parser could not
--                         parse. This should typically be an empty
--                         list after a successful parse
--
runP :: Parser symb err res -> (Reporter symb err) -> [symb] -> Either err (res, [symb])
runP p r xs = case p r xs of
                  Right res     -> Right res
                  Left (_, err) -> Left err
    
-------------------------------------------------------------------------------------------------------
--  Primitive parsers
-------------------------------------------------------------------------------------------------------

--
-- This is the most trivial parser. It always returns a success
-- without consuming any input.
--
ok :: res -> Parser symb err res
ok s _ xs = Right (s, xs)

--
-- Opposite of `ok`; it always fails.
--
failure :: Parser symb err res
failure r xs = Left $ regErr $ r xs

--
-- Represents the epsilon production in BNF
--
epsilon :: Parser symb err ()
epsilon = ok ()

--
-- This parser fails unless all input symbols have been consumed.
--
-- Typically used to test for garbage symbols in the input
--
atEOF :: Parser symb err ()
atEOF _ [] = Right ((), [])
atEOF r xs = Left $ regErr $ r xs

--
-- This parser succeeds if the given predicate is satisfied.
--
satisfy :: (symb -> Bool) -> Parser symb err symb
satisfy _    r []              = Left $ regErr $ r []
satisfy p r (x:xs) | p x       = Right (x, xs)
                   | otherwise = Left $ regErr $ r (x:xs)

--
-- Succeed if the next input symbol matches the given one.
--
symbol :: Eq symb => symb -> Parser symb err symb
symbol s = satisfy (==s)

--
-- Succeed if the next input symbol matches one of the given symbols.
--
symbols :: Eq symb => [symb] -> Parser symb err symb
symbols symbs = satisfy (`elem` symbs)

--
-- Succeed if the given list of symbols is a prefix of the input list.
--
match :: Eq symb => [symb] -> Parser symb err [symb]
match pfix r xs | isPrefixOf pfix xs = Right (pfix, drop (length pfix) xs)
                | otherwise          = Left $ regErr $ r xs

--
-- Given a function that works like the `span` function of the
-- Prelude, succeed and return the matched fprefix of the input table.
--
-- Example: `spanP (span isDigit)` will return the prefix of the input
--          list that are digits.
--
spanP :: ([symb] -> ([symb], [symb])) -> Parser symb err [symb]
spanP spanner _ xs = Right $ spanner xs
              
--
-- Replace the current reporter with a customized one.
--
report :: Reporter s e -> Parser s e r -> Parser s e r
report newR p _ xs = p newR xs

--
-- Create a cut
--
cut :: Parser symb err res -> Parser symb err res
cut p r xs = case p r xs of
                 Right res -> Right res
                 Left ce   -> Left $ mkCut ce

-------------------------------------------------------------------------------------------------------
--  Basic parsing combinators
-------------------------------------------------------------------------------------------------------

--
-- Parsing combinators are used to construct more complex parsers from
-- simpler ones.
--
-- Typically one wants to emulate a (E)BNF grammar. Thus there are two
-- main types of combinators: Sequence combinators that parses the
-- elements of a production and fails if one of them fails. Choice
-- combinators are used to choose between production rules. If one of
-- the simpler parsers succeeds, the larger parser succeeds.
--

infixl 6 `ap`
infixl 6 `chk`
infixr 4 <||>

--
-- This combinator takes two parsers, One that returns a constructor
-- function (a->b) and one that produces the argument to the
-- constructor. It fails if either one fails.
--
-- `ap` comes from 'apply', as in apply a to the function.
--
ap :: Parser symb err (a->b) -> Parser symb err a -> Parser symb err b
ap pf pa r xs = do (f, xs')  <- pf r xs
                   (a, xs'') <- pa r xs'
                   return $ (f a, xs'')

--
-- This is similar to `ap` but disregards the parse result of the
-- right parser. Thus it is possible to disregard superfluous
-- terminals.
--
-- Examples:
--     Parse an addition rule 'addition <-- integer '+' integer:
--        pAdd = ok (\a b -> a+b) `ap` pInt `chk` pPlus `ap` pInt
--
--     `pInt` and `pPlus` are parsers that parses integers and plus
--     symbols respectivelly.
--
chk :: Parser symb err a -> Parser symb err b -> Parser symb err a
chk pa pb r xs = do (a, xs') <- pa r xs
                    (_, xs'') <- pb r xs'
                    return $ (a, xs'')

--
-- This combinator implements choice between two production rules.
--
-- Example: Given the grammar G <-- 'A' | 'B' which either accepts an 'A' or 'B'.
--          Given we have the parsers pA and pB, we can implement the grammar as:
--              pG = pA <||> pB
--
(<||>) :: Ord err => Parser symb err res -> Parser symb err res -> Parser symb err res
(<||>) p1 p2 r xs = case p1 r xs of
                        Right res            -> Right res
                        Left  e1 | isCut e1  -> Left e1
                                 | otherwise -> case p2 r xs of
                                                    Right res -> Right res
                                                    Left  e2  -> Left $ max e1 e2

--
-- expErr p q works as follows: If p fails the entire parser fails. If
-- p succeeds, parser q is applied to the input list.
--
expErr :: Parser symb err res -> Parser symb err res' -> Parser symb err res'
expErr fp sp r xs = case fp r xs of
                        Left err -> Left err
                        Right _  -> sp r xs

-------------------------------------------------------------------------------------------------------
--   Non-primitive parsers and combinators
-------------------------------------------------------------------------------------------------------

--
-- These are combinators and parsers that implement common operations
-- but can be defined using the primitive counterparts.
--

--
-- Apply a parser zero or more times. Each result is returned in a
-- list
--
many :: Ord err => Parser symb err a -> Parser symb err [a]
many p = ok (:) `ap` p `ap` many p <||> ok []

--
-- Apply a parser at least one time. Each result is returned in a
-- list
--
some :: Ord err => Parser symb err a -> Parser symb err [a]
some p = ok (:) `ap` p `ap` many p

--
-- Take input symbols as long as the predicate is satisfied.
--
takeMany :: Ord err => (symb -> Bool) -> Parser symb err [symb]
takeMany p = many $ satisfy p

--
-- Like `takeMany` but the predicate must be satisfied at least once.
--
takeSome :: Ord err => (symb -> Bool) -> Parser symb err [symb]
takeSome p = some $ satisfy p

--
-- If the given parser fails, return the default value.
--
optP :: Ord err => a -> Parser symb err a -> Parser symb err a
optP def p = p <||> ok def

--
-- Parse at most once
--
cond :: Ord err => Parser symb err a -> Parser symb err [a]
cond p = ok (:[]) `ap` p <||> ok []

--
-- Multiple choice. Returns the result of the first parser in the list
-- that cucceeds.
--
anyOf :: Ord err => [Parser symb err res] -> Parser symb err res
anyOf = foldr1 (<||>)

--
-- Match the first list of symbols that is a prefix of the input list.
--
matchSet :: (Ord err, Eq symb) => [[symb]] -> Parser symb err [symb]
matchSet = anyOf . map match

--
-- Replace the current reporter, then fail.
--
repFail :: Reporter symb err -> Parser symb err res
repFail r = report r $ failure
