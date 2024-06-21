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

module BuiltIns (
                 builtIns,
                 getBuiltIn
                ) where

import CoreTypes
import Interpreter

-- ====================================================================================================

builtIns :: Env
builtIns =
    map defBI [
               defBinIntOp "+" (+),
               defBinIntOp "-" (-),
               defBinIntOp "*" (*),
               defBinIntOp "/" div,
               defBinCmpOp "=" (==),
               defBinCmpOp "<" (<),
               defBinCmpOp ">" (>),
               defBinCmpOp "<=" (<=),
               defBinCmpOp ">=" (>=),
               defBinCmpOp "<>" (/=),
               defUnBoolOp "~" not,
               defBinBoolOp "and" (&&),
               defBinBoolOp "or" (||),
               defStash,
               defApply,
               defCond,
               defDrop,
               defSwap,
               defRot,
               defOver,
               defDup,
               defClear,
               defDepth,
               defPrint,
               defAppend
              ]

defBI :: Value -> (Name, Value)
defBI op@(ValOp name _) = (name, op)
defBI op                = error $ "INTERNAL ERROR: A builtin is not a ValOp: '" ++ show op ++ "'"


getBuiltIn :: Name -> Maybe Value
getBuiltIn name = lookup name builtIns


defBinIntOp :: Name -> (Integer -> Integer -> Integer) -> Value
defBinIntOp name f = defBinOp name $ numBinOp name f

defBinCmpOp :: Name -> (Integer -> Integer -> Bool) -> Value
defBinCmpOp name f = defBinOp name $ cmpBinOp name f

defUnBoolOp :: Name -> (Bool -> Bool) -> Value
defUnBoolOp name f = defUnOp name $ boolUnOp name f

defBinBoolOp :: Name -> (Bool -> Bool -> Bool) -> Value
defBinBoolOp name f = defBinOp name $ boolBinOp name f

defBinOp :: Name -> (Value -> Value -> Result Value) -> Value
defBinOp name f =
    defOp name $ \cxt ->
        case stack cxt of
            x : y : stack1 -> do { val <- f y x; return cxt{stack = val : stack1}; }
            _              -> stackUnderflowError name
                         
defUnOp :: Name -> (Value -> Result Value) -> Value
defUnOp name f = 
    defOp name $ \cxt ->
        case cxt of
            cxt1@Cxt{stack = x : s} -> do { val <- f x; return cxt1{stack = val : s}; }
            _                       -> stackUnderflowError name

numBinOp :: Name -> (Integer -> Integer -> Integer) -> Value -> Value -> Result Value
numBinOp _    f (ValInt v1) (ValInt v2) = Right $ ValInt $ f v1 v2
numBinOp name _ x           y           = typeError2 name "numerical arguments of same type" x y

cmpBinOp :: Name -> (Integer -> Integer -> Bool) -> Value -> Value -> Result Value
cmpBinOp _    f (ValInt v1) (ValInt v2) = Right $ bool2Truth $ f v1 v2
cmpBinOp name _ x           y           = typeError2 name "comparable arguments of same type" x y

boolBinOp :: Name -> (Bool -> Bool -> Bool) -> Value -> Value -> Result Value
boolBinOp _ f v1 v2 = Right $ bool2Truth $ f (truth2Bool v1) (truth2Bool v2)

boolUnOp :: Name -> (Bool -> Bool) -> Value -> Result Value
boolUnOp _ f v1 = Right $ bool2Truth $ f (truth2Bool v1)


bool2Truth :: Bool -> Value
bool2Truth False = ValInt 0
bool2Truth True  = ValInt 1

truth2Bool :: Value -> Bool
truth2Bool (ValInt 0)    = False
truth2Bool (ValStack []) = False
truth2Bool _             = True

-- ====================================================================================================

defOp :: Name -> (Cxt -> Result Cxt) -> Value
defOp name op = ValOp name $ \cxt -> return $ op cxt


defCond :: Value
defCond =
    ValOp "?" $ \cxt@Cxt{stack = s0} ->
        case s0 of
           elsePart : thenPart : predicate : s1 ->
               do result <- runValues cxt{stack = s1} [predicate, defApply]
                  ifOk result $ \cxt1@Cxt{stack = s2} ->
                      ifOk (getValInt $ head s2) $ \predInt ->
                          runValues cxt1{stack = tail s2} $
                              if predInt /= 0
                                  then [thenPart, defApply]
                                  else [elsePart, defApply]
           _ ->
               return $ stackUnderflowError "?"

defDrop :: Value
defDrop = defOp "drop" $ \cxt@Cxt{stack = s0} ->
              case s0 of
                  _ : s1 -> Right cxt{stack = s1}
                  _      -> stackUnderflowError "drop"

defSwap :: Value
defSwap = defOp "swap" $ \cxt@Cxt{stack = s0} ->
              case s0 of
                  x : y : s1 -> Right cxt{stack = y : x : s1}
                  _          -> stackUnderflowError "swap"


defRot :: Value
defRot = defOp "rot" $ \cxt@Cxt{stack = s0} ->
             case s0 of
                 x : y : z : s1 -> Right cxt{stack = z : y : x : s1}
                 _              -> stackUnderflowError "rot"

defOver :: Value
defOver = defOp "over" $ \cxt@Cxt{stack = s0} ->
              case s0 of
                  x : y : s1 -> Right cxt{stack = y : x : y : s1}
                  _          -> stackUnderflowError "over"

defDup :: Value
defDup = defOp "dup" $ \cxt@Cxt{stack = s0} ->
             case s0 of
                 x : s1 -> Right cxt{stack = x : x : s1}
                 _      -> stackUnderflowError "dup"

defClear :: Value
defClear = defOp "clear" $ \cxt -> Right cxt{stack = []}

defDepth :: Value
defDepth = defOp "depth" $ \cxt@Cxt{stack = s} ->
               let
                   depth = (ValInt $ toInteger $ length s)
               in
                   Right cxt{stack = depth : s}
                         
defPrint :: Value
defPrint = ValOp "print" $ \cxt@Cxt{stack = s0} ->
          case s0 of
              val : s1 -> do putStrLn $ show val
                             return $  Right cxt{stack = s1}
              _        -> return $ stackUnderflowError "print"
                                 
defStash :: Value
defStash = defOp ";" $ \cxt@Cxt{stack = s0} ->
                       case s0 of
                           ValAtom key : val : s1 ->
                               insertEnv cxt{stack = s1} key val 
                           key : _ : _ ->
                               typeError1 ";" "an atom as key for" key
                           _ ->
                               stackUnderflowError ";"

defAppend :: Value
defAppend  =
    defOp "++" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            ValStack s1 : ValStack s2 : s3 ->
                Right cxt{stack = ValStack (s2 ++ s1) : s3}
            ValString str1 : ValString str2 : s3 ->
                Right cxt{stack = ValString (str2 ++ str1) : s3}
            v1 : v2 : _ ->
                typeError2 "++" "either two stacks or strings" v2 v1
            _ ->
                stackUnderflowError "++"

-- ====================================================================================================
