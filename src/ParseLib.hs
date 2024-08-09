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

module ParseLib where

import Data.List(isPrefixOf)
    
-------------------------------------------------------------------------------------------------------

type Reporter symb err = [symb] -> err

constR :: e -> Reporter a e
constR e _ = e
             
-------------------------------------------------------------------------------------------------------

type Cut = Bool

regErr :: err -> (Cut, err)
regErr e = (False, e)

isCut :: (Cut, err) -> Bool
isCut (c, _) = c

mkCut :: (Cut, err) -> (Cut, err)
mkCut (_, e) = (True, e)

-------------------------------------------------------------------------------------------------------

type Parser symb err res = (Reporter symb err) -> [symb] -> Either (Cut, err) (res, [symb])
    
-------------------------------------------------------------------------------------------------------


ok :: res -> Parser symb err res
ok s _ xs = Right (s, xs)

failure :: Parser symb err res
failure r xs = Left $ regErr $ r xs

epsilon :: Parser symb err ()
epsilon = ok ()

atEOF :: Parser symb err ()
atEOF _ [] = Right ((), [])
atEOF r xs = Left $ regErr $ r xs


satisfy :: (symb -> Bool) -> Parser symb err symb
satisfy _    r []              = Left $ regErr $ r []
satisfy p r (x:xs) | p x       = Right (x, xs)
                   | otherwise = Left $ regErr $ r (x:xs)

symbol :: Eq symb => symb -> Parser symb err symb
symbol s = satisfy (==s)

symbols :: Eq symb => [symb] -> Parser symb err symb
symbols symbs = satisfy (`elem` symbs)

match :: Eq symb => [symb] -> Parser symb err [symb]
match pfix r xs | isPrefixOf pfix xs = Right (pfix, drop (length pfix) xs)
                | otherwise          = Left $ regErr $ r xs

spanP :: ([symb] -> ([symb], [symb])) -> Parser symb err [symb]
spanP spanner _ xs = Right $ spanner xs
                                     

runP :: Parser symb err res -> (Reporter symb err) -> [symb] -> Either err (res, [symb])
runP p r xs = case p r xs of
                  Right res     -> Right res
                  Left (_, err) -> Left err
              

report :: Reporter s e -> Parser s e r -> Parser s e r
report newR p _ xs = p newR xs

cut :: Parser symb err res -> Parser symb err res
cut p r xs = case p r xs of
                 Right res -> Right res
                 Left ce   -> Left $ mkCut ce

-------------------------------------------------------------------------------------------------------


infixl 6 `ap`
infixl 6 `chk`
infixr 4 <||>


ap :: Parser symb err (a->b) -> Parser symb err a -> Parser symb err b
ap pf pa r xs = do (f, xs')  <- pf r xs
                   (a, xs'') <- pa r xs'
                   return $ (f a, xs'')

chk :: Parser symb err a -> Parser symb err b -> Parser symb err a
chk pa pb r xs = do (a, xs') <- pa r xs
                    (_, xs'') <- pb r xs'
                    return $ (a, xs'')

(<||>) :: Ord err => Parser symb err res -> Parser symb err res -> Parser symb err res
(<||>) p1 p2 r xs = case p1 r xs of
                        Right res            -> Right res
                        Left  e1 | isCut e1  -> Left e1
                                 | otherwise -> case p2 r xs of
                                                    Right res -> Right res
                                                    Left  e2  -> Left $ max e1 e2

expErr :: Parser symb err res -> Parser symb err res' -> Parser symb err res'
expErr fp sp r xs = case fp r xs of
                        Left err -> Left err
                        Right _  -> sp r xs

-------------------------------------------------------------------------------------------------------



many :: Ord err => Parser symb err a -> Parser symb err [a]
many p = ok (:) `ap` p `ap` many p <||> ok []

some :: Ord err => Parser symb err a -> Parser symb err [a]
some p = ok (:) `ap` p `ap` many p


takeMany :: Ord err => (symb -> Bool) -> Parser symb err [symb]
takeMany p = many $ satisfy p

takeSome :: Ord err => (symb -> Bool) -> Parser symb err [symb]
takeSome p = some $ satisfy p

optP :: Ord err => a -> Parser symb err a -> Parser symb err a
optP def p = p <||> ok def


cond :: Ord err => Parser symb err a -> Parser symb err [a]
cond p = ok (:[]) `ap` p <||> ok []

anyOf :: Ord err => [Parser symb err res] -> Parser symb err res
anyOf = foldr1 (<||>)

matchSet :: (Ord err, Eq symb) => [[symb]] -> Parser symb err [symb]
matchSet = anyOf . map match

repFail :: Reporter symb err -> Parser symb err res
repFail r = report r $ failure
