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

type Parser symb err res = (Reporter symb err) -> [symb] -> Either err (res, [symb])
    
-------------------------------------------------------------------------------------------------------


ok :: res -> Parser symb err res
ok s _ xs = Right (s, xs)

failure :: Parser symb err res
failure r xs = Left $ r xs


epsilon :: Parser symb err ()
epsilon = ok ()

atEOF :: Parser symb err ()
atEOF _ [] = Right ((), [])
atEOF r xs = Left $ r xs


satisfy :: (symb -> Bool) -> Parser symb err symb
satisfy _    r []              = Left $ r []
satisfy p r (x:xs) | p x       = Right (x, xs)
                   | otherwise = Left $ r (x:xs)

symbol :: Eq symb => symb -> Parser symb err symb
symbol s = satisfy (==s)

symbols :: Eq symb => [symb] -> Parser symb err symb
symbols symbs = satisfy (`elem` symbs)

match :: Eq symb => [symb] -> Parser symb err [symb]
match pfix r xs | isPrefixOf pfix xs = Right (pfix, drop (length pfix) xs)
                | otherwise          = Left $ r xs

spanP :: ([symb] -> ([symb], [symb])) -> Parser symb err [symb]
spanP spanner _ xs = Right $ spanner xs
                                     

runP :: Parser symb err res -> (Reporter symb err) -> [symb] -> Either err (res, [symb])
runP p r xs = p r xs
              

report :: Reporter s e -> Parser s e r -> Parser s e r
report newR p _ xs = p newR xs

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
                        Right res -> Right res
                        Left  e1  -> case p2 r xs of
                                         Right res -> Right res
                                         Left  e2  -> Left $ max e1 e2



    
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

