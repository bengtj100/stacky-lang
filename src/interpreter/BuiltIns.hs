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
-- This module defines the built-in operations of Stacky.
--
-- It provides a default environment that contains all operations. The
-- operations themselves are defined later in the file.
--
-- Operations start with 'def' helper functions do not. Helper
-- functions that are only used in one operation (or type of
-- operations) are located together with the operations. More general
-- helpers are located towards the end of the module.
--
-- Operations are described in the Language reference.
--
-------------------------------------------------------------------------------------------------------

module BuiltIns (
                 builtIns
                ) where

-- System modules
import Data.Char(chr, ord)
import Text.Read(readMaybe)

import Control.Exception(catch)

-- Base modules
import CoreTypes(Cxt(..), insertEnv, insertEnvGlobal, updateEnv, updateEnvGlobal,
                 Env,
                 Name,
                 Result,        ifOk, stackUnderflowError, typeError1, typeError2, newErrPos,
                 Value(..),     isComparable, getValPos, isSequence, valueType, valueTypeSize)

import Position(Position(..),   mkPos, noPos)

import InputOutput(getLines)

import LibraryPath(findLibModule)

-- Interpreter modules
import Interpreter(defApply, runValues, runLocalValues)

-- Frontend modules
import FrontEnd(parseLine, parseFile)

-------------------------------------------------------------------------------------------------------
--  Main API functions
-------------------------------------------------------------------------------------------------------

builtIns :: Env
builtIns =
    map defBI [
               -- Arithmetic operations
               defBinOp "+" valAdd,
               defBinOp "-" valSub,
               defBinOp "*" valMult,
               defBinOp "/" valDiv,
               defBinOp "%" valRem,
               defBinOp "pow" valPow,
               defUnOp  "floor" valFloor,
               defUnOp  "float" valFloat,
               defUnOp  "abs"   valAbs,
               defUnOp  "sign"  valSign,
               defMath "exp" exp,
               defMath "sqrt" sqrt,
               defMath "log" log,
               defMath "log2" (logBase 2),
               defMath "log10" (logBase 10),
               defMath "sin" sin,
               defMath "tan" tan,
               defMath "cos" cos,
               defMath "asin" asin,
               defMath "atan" atan,
               defMath "acos" acos,
               defMath "sinh" sinh,
               defMath "tanh" tanh,
               defMath "cosh" cosh,
               defMath "asinh" asinh,
               defMath "atanh" atanh,
               defMath "acosh" acosh,
               defUnOp "!" factorial,
               
               -- Comparison operations
               defBinCmpOp "=" (==),
               defBinCmpOp "<>" (/=),
               defBinCmpOp "<" (<),
               defBinCmpOp ">" (>),
               defBinCmpOp "<=" (<=),
               defBinCmpOp ">=" (>=),

               -- Boolean operations
               defUnBoolOp "~" not,
               defBinBoolOp "and" (&&),
               defBinBoolOp "or" (||),

               -- Control operations
               defStash, defGlobal,
               defAssign, defUpdate,
               defCond,

               -- Stack operations
               defDrop,  defSwap,  defRot,  defLRot,  defOver,  defDup, defClear, defDepth,
               defNDrop, defNSwap, defNRot, defNLRot, defNOver, defNDup,

               -- Sequence operations
               defAppend, defLength,
               defToList, defFromList,
               defToString, defFromString,
               defReverse, defToStr, defSlice,
               defChr, defOrd,
                       
               -- Input/output operations
               defPrint, defPut, defPutLn, defPrompt, defReadFile,

               -- Reflection/introspection operations
               defApply, defApplyList, defEval, defImport, defEnv, defTypeOf,
               defTypeInfo, defExpectType, defExpectDepth, defThrow, defCatch,
               defCallPos
              ]

-------------------------------------------------------------------------------------------------------
--  Arithmetic operations
-------------------------------------------------------------------------------------------------------

--
-- Help define a mathematical function. Uses the Haskell built-in
-- functions w. type Double -> Double.
--
defMath :: Name -> (Double -> Double) -> Value
defMath name f = defUnOp name $ valFloatFun name f


type IntOp   = Integer -> Integer -> Integer
type FloatOp = Double  -> Double  -> Double

type IntFun   = Integer -> Integer
type FloatFun = Double  -> Double

valAdd :: Value -> Value -> Result Value
valAdd x y = valOp "+" (+) (+) x y

valSub :: Value -> Value -> Result Value
valSub x y = valOp "-" (-) (-) x y

valMult :: Value -> Value -> Result Value
valMult x y = valOp "*" (*) (*) x y

valDiv :: Value -> Value -> Result Value
valDiv _ (ValInt p 0) = newErrPos p "Division by zero"
valDiv x y            = valOp "/" div (/) x y

valRem :: Value -> Value -> Result Value
valRem _ (ValInt   p 0  ) = newErrPos p "Zero Reminder"
valRem _ (ValFloat p 0.0) = newErrPos p "Zero Reminder"
valRem x y                = valOp "/" rem floatRem x y

floatRem :: Double -> Double -> Double
floatRem fx fy = fromIntegral (x `rem` y)
                 where x = floor fx :: Integer
                       y = floor fy :: Integer

valPow :: Value -> Value -> Result Value
valPow x y = valOp "pow" intPow (**) x y

intPow :: Integer -> Integer -> Integer
intPow x n | n < 0          = 0
           | n < 1          = 1
           | n `rem` 2 == 1 = x * intPow x (n-1)
           | otherwise      = x2 * x2
           where
               x2 = intPow x (n `div` 2)

valAbs, valSign :: Value -> Result Value
valAbs  = valFun "abs"  abs    abs
valSign = valFun "sign" signum signum

valOp :: Name -> IntOp -> FloatOp -> Value -> Value -> Result Value
valOp _    iop _   (ValInt pos x)   (ValInt _ y)   = Right $ ValInt   pos (x `iop` y)
valOp _    _   fop (ValFloat pos x) (ValFloat _ y) = Right $ ValFloat pos (x `fop` y)
valOp _    _   fop (ValInt pos x)   (ValFloat _ y) = Right $ ValFloat pos (fromIntegral x `fop` y)
valOp _    _   fop (ValFloat pos x) (ValInt _ y)   = Right $ ValFloat pos (x `fop` fromIntegral y)
valOp name _   _   vx               vy             = typeError2 vx name "numerical arguments" vx vy

valFun :: Name -> IntFun -> FloatFun -> Value -> Result Value
valFun _    iFun _    (ValInt pos x)   = Right $ ValInt   pos (iFun x)
valFun _    _    fFun (ValFloat pos x) = Right $ ValFloat pos (fFun x)
valFun name _    _    vx               = typeError1 vx name "numerical argument" vx

valFloatFun :: Name -> (Double -> Double) -> Value -> Result Value
valFloatFun _    f (ValInt pos val)   = Right $ ValFloat pos (f $ fromIntegral val)
valFloatFun _    f (ValFloat pos val) = Right $ ValFloat pos (f val)
valFloatFun name _ val                = typeError1 val name "numerical argument" val
                                      
valFloor :: Value -> Result Value
valFloor (ValFloat pos fval) = Right $ ValInt pos (floor fval)
valFloor (ValInt   pos ival) = Right $ ValInt pos ival
valFloor val                 = typeError1 val "floor" "numerical argument" val

valFloat :: Value -> Result Value
valFloat vf@(ValFloat _ _)      = Right vf
valFloat    (ValInt   pos ival) = Right $ ValFloat pos (fromIntegral ival)
valFloat    (ValString pos str) = Right $ ValFloat pos (fromString str)
valFloat val                    = typeError1 val "float" "numerical or string argument" val

fromString :: String -> Double
fromString str =
    case readMaybe str of
        Just f -> f
        Nothing -> 0.0

-------------------------------------------------------------------------------------------------------

--
-- Define a unary operation. The operation may fail with an error.
--
defUnOp :: Name -> (Value -> Result Value) -> Value
defUnOp name f = 
    defOp name $ \cxt ->
        case cxt of
            cxt1@Cxt{stack = x : s} -> do { val <- f x; return cxt1{stack = val : s}; }
            _                       -> stackUnderflowError ValNoop name

-------------------------------------------------------------------------------------------------------

--
-- Define a binary operation. The operation may fail with an error.
--
defBinOp :: Name -> (Value -> Value -> Result Value) -> Value
defBinOp name f =
    defOp name $ \cxt ->
        case stack cxt of
            x : y : stack1 -> do { val <- f y x; return cxt{stack = val : stack1}; }
            _              -> stackUnderflowError ValNoop name

-------------------------------------------------------------------------------------------------------

--
-- Helper function that computes factorials. (!)
--
factorial :: Value -> Result Value
factorial (ValInt pos n)   = Right $ ValInt pos $ fact n
factorial (ValFloat pos n) = Right $ ValInt pos $ fact $ floor n
factorial val              = typeError1 val "!" "expects an integer or float" val
--
-- Compute The factorial n! of an integer.
--
-- In case someone wonders, this was the fastest way without resorting
-- to exotic algorithms. Even faster than an optimized tail-recursive
-- function...
--
fact :: Integer -> Integer
fact n = product [1..n]

-------------------------------------------------------------------------------------------------------
--  Comparison operations
-------------------------------------------------------------------------------------------------------

--
-- Define a binary comparison operation
--
defBinCmpOp :: Name -> (Value -> Value -> Bool) -> Value
defBinCmpOp name f = defBinOp name $ cmpBinOp name f

cmpBinOp :: Name -> (Value -> Value -> Bool) -> Value -> Value -> Result Value
cmpBinOp name f x y | isComparable x y = Right $ bool2Truth (getValPos x) $ f x y
                    | otherwise        = typeError2 x name "comparable arguments" x y

-------------------------------------------------------------------------------------------------------
--  Boolean operations
-------------------------------------------------------------------------------------------------------

--
-- Define a unary boolean operation.
--
defUnBoolOp :: Name -> (Bool -> Bool) -> Value
defUnBoolOp name f = defUnOp name $ boolUnOp name f

boolUnOp :: Name -> (Bool -> Bool) -> Value -> Result Value
boolUnOp _ f v1 = Right $ bool2Truth (getValPos v1) $ f (truth2Bool v1)

-------------------------------------------------------------------------------------------------------

--
-- Define a binary boolean operation.
--
defBinBoolOp :: Name -> (Bool -> Bool -> Bool) -> Value
defBinBoolOp name f = defBinOp name $ boolBinOp name f

boolBinOp :: Name -> (Bool -> Bool -> Bool) -> Value -> Value -> Result Value
boolBinOp _ f v1 v2 = Right $ bool2Truth (getValPos v1) $ f (truth2Bool v1) (truth2Bool v2)

-------------------------------------------------------------------------------------------------------
--  Control operations
-------------------------------------------------------------------------------------------------------

defStash :: Value
defStash = stash ";" insertEnv

defGlobal :: Value
defGlobal = stash "global" insertEnvGlobal

defAssign :: Value
defAssign = stash ";=;" updateEnv

defUpdate :: Value
defUpdate = stash "UPDATE" updateEnvGlobal
            
--
-- Stash a value using the provided insert function.
--
stash :: Name -> (Cxt -> Name -> Value -> Result Cxt) -> Value
stash name insert =
    defOp name $ \cxt@Cxt{stack = s0} ->
                 case s0 of
                     ValAtom _ key : val : s1 ->
                         insert cxt{stack = s1} key val 
                     key : _ : _ ->
                         typeError1 key name "an atom as key for" key
                     _ ->
                         stackUnderflowError ValNoop name

-------------------------------------------------------------------------------------------------------

defCond :: Value
defCond =
    ValOp noPos "?" $ \cxt@Cxt{stack = s0} ->
        case s0 of
           elsePart : thenPart : predicate : s1 ->
               do result <- runValues cxt{stack = s1} [predicate, defApply]
                  ifOk result $ \cxt1@Cxt{stack = s2} ->
                      runValues cxt1{stack = safeTail s2} $
                                if   truth2Bool (safeHead s2)
                                then [thenPart, defApply]
                                else [elsePart, defApply]
           _ ->
               return $ stackUnderflowError ValNoop "?"

--
-- A head function that cannot fail. Returns boolean false on an empty
-- list.
--
safeHead :: [Value] -> Value
safeHead []    = ValInt noPos 0
safeHead (x:_) = x

-- Safely take the tail of a list. Just return an empty list if the
-- input is empty.
--
safeTail :: [Value] -> [Value]
safeTail []     = []
safeTail (_:xs) = xs

-------------------------------------------------------------------------------------------------------
--  Stack operations
-------------------------------------------------------------------------------------------------------
                  
defDrop :: Value
defDrop = defOp "drop" $ \cxt@Cxt{stack = s0} ->
              case s0 of
                  _ : s1 -> Right cxt{stack = s1}
                  _      -> stackUnderflowError ValNoop "drop"

-------------------------------------------------------------------------------------------------------

defSwap :: Value
defSwap = defOp "swap" $ \cxt@Cxt{stack = s0} ->
              case s0 of
                  x : y : s1 -> Right cxt{stack = y : x : s1}
                  _          -> stackUnderflowError ValNoop "swap"


-------------------------------------------------------------------------------------------------------

defRot :: Value
defRot = defOp "rot" $ \cxt@Cxt{stack = s0} ->
             case s0 of
                 x : y : z : s1 -> Right cxt{stack = z : x : y : s1}
                 _              -> stackUnderflowError ValNoop "rot"

-------------------------------------------------------------------------------------------------------

defLRot :: Value
defLRot = defOp "lrot" $ \cxt@Cxt{stack = s0} ->
             case s0 of
                 x : y : z : s1 -> Right cxt{stack = y : z : x : s1}
                 _              -> stackUnderflowError ValNoop "rot"

-------------------------------------------------------------------------------------------------------

defOver :: Value
defOver = defOp "over" $ \cxt@Cxt{stack = s0} ->
              case s0 of
                  x : y : s1 -> Right cxt{stack = y : x : y : s1}
                  _          -> stackUnderflowError ValNoop "over"

-------------------------------------------------------------------------------------------------------

defDup :: Value
defDup = defOp "dup" $ \cxt@Cxt{stack = s0} ->
             case s0 of
                 x : s1 -> Right cxt{stack = x : x : s1}
                 _      -> stackUnderflowError ValNoop "dup"

-------------------------------------------------------------------------------------------------------

defClear :: Value
defClear = defOp "clear" $ \cxt -> Right cxt{stack = []}

-------------------------------------------------------------------------------------------------------

defDepth :: Value
defDepth = defOp "depth" $ \cxt@Cxt{stack = s} ->
               let
                   depth = (ValInt noPos $ toInteger $ length s)
               in
                   Right cxt{stack = depth : s}                         

-------------------------------------------------------------------------------------------------------

defNDrop :: Value
defNDrop = nStackOp "ndrop" $ \_ st -> st
    
-------------------------------------------------------------------------------------------------------

defNSwap :: Value
defNSwap = nStackOp "nswap" $ \pfix st -> reverse pfix ++ st

-------------------------------------------------------------------------------------------------------

defNRot :: Value
defNRot = nStackOp "nrot" $ \pfix st ->
          if null pfix
             then st
             else (last pfix) : (init pfix) ++ st

-------------------------------------------------------------------------------------------------------

defNLRot :: Value
defNLRot = nStackOp "nlrot" $ \pfix st ->
           if null pfix
              then st
              else (tail pfix) ++ (head pfix : st)

-------------------------------------------------------------------------------------------------------

defNOver :: Value
defNOver = nStackOp "nover" $ \pfix st ->
           if null pfix
              then st
              else (last pfix) : pfix ++ st

-------------------------------------------------------------------------------------------------------

defNDup :: Value
defNDup = nStackOp "ndup" $ \pfix st -> pfix ++ pfix ++ st

-------------------------------------------------------------------------------------------------------

--
-- Helper for stack operations that take a variable argument to
-- describe the number of elements to operate on, such as `ndrop`
--
-- The number of elements is on the top of the stack and that number
-- of elements are handed to the handler function. The result of the
-- handler is then pushed onto the stack.
--
nStackOp :: String -> ([Value] -> [Value] -> [Value]) -> Value
nStackOp name handler =
    defOp name $ \cxt@Cxt{stack = s0} ->
           case s0 of
               ValInt _ n : s1 | n >= 0 ->
                   let
                       nInt = fromIntegral n
                   in
                       if length s1 >= nInt then
                           let (pfix, s2) = splitAt nInt s1
                               s3         = handler pfix s2
                           in Right cxt{stack = s3}
                       else
                           stackUnderflowError ValNoop name
               x : _ ->
                   typeError1 x name "a non-negative integer" x
               [] ->
                   stackUnderflowError ValNoop name

-------------------------------------------------------------------------------------------------------
--  Sequence operations
-------------------------------------------------------------------------------------------------------

defAppend :: Value
defAppend  =
    defOp "++" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            ValList p xs : ValList _ ys : s3 ->
                Right cxt{stack = ValList p (ys ++ xs) : s3}
            ValString p str1 : ValString _ str2 : s3 ->
                Right cxt{stack = ValString p (str2 ++ str1) : s3}
            v1 : v2 : _ ->
                typeError2 v1 "++" "either two lists or strings" v2 v1
            _ ->
                stackUnderflowError ValNoop "++"

-------------------------------------------------------------------------------------------------------

defLength :: Value
defLength  =
    defOp "length" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            ValList p xs : s3 ->
                Right cxt{stack = ValInt p (toInteger $ length xs) : s3}
            ValString p str : s3 ->
                Right cxt{stack = ValInt p (toInteger $ length str) : s3}
            v1 : _ ->
                typeError1 v1 "length" "either a list or string" v1
            _ ->
                stackUnderflowError ValNoop "length"


-------------------------------------------------------------------------------------------------------

defToList :: Value
defToList =
    nStackOp "toList" $ \pfix st -> ValList noPos (reverse pfix) : st

-------------------------------------------------------------------------------------------------------

defFromList :: Value
defFromList  =
    defOp "fromList" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            ValList p xs : s1 ->
                let len = ValInt p $ toInteger $ length xs
                in  Right cxt{stack = len : reverse xs ++ s1}
            v1 : _ ->
                typeError1 v1 "fromList" "a list" v1
            _ ->
                stackUnderflowError ValNoop "fromList"
                             
-------------------------------------------------------------------------------------------------------

defToString :: Value
defToString =
    nStackOp "toString" $ \pfix st ->
        ValString noPos (concat $ map toString $ reverse pfix) : st

-------------------------------------------------------------------------------------------------------

defFromString :: Value
defFromString  =
    defOp "fromString" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            ValString p str : s1 ->
                let len = ValInt p $ toInteger $ length str
                    strs = [ ValString p [c] | c <- reverse str ]
                in  Right cxt{stack = len : strs ++ s1}
            v1 : _ ->
                typeError1 v1 "fromString" "a string" v1
            _ ->
                stackUnderflowError ValNoop "fromString"

-------------------------------------------------------------------------------------------------------

defReverse :: Value
defReverse  =
    defOp "reverse" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            ValList p xs : s3 ->
                Right cxt{stack = ValList p (reverse xs) : s3}
            ValString p str : s3 ->
                Right cxt{stack = ValString p (reverse str) : s3}
            v1 : _ ->
                typeError1 v1 "reverse" "either a list or string" v1
            _ ->
                stackUnderflowError ValNoop "reverse"

-------------------------------------------------------------------------------------------------------

defToStr :: Value
defToStr  =
    defOp "toStr" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            val : s3 ->
                Right cxt{stack = ValString (getValPos val) (show val) : s3}
            _ ->
                stackUnderflowError ValNoop "toStr"

-------------------------------------------------------------------------------------------------------

defSlice :: Value
defSlice =
    defOp "slice" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            ValInt _ to : ValInt _ from  : ValString pos str : s3 ->
                doSlice from to str pos $ \res -> Right cxt{stack = ValString pos res : s3}
            ValInt _ to : ValInt _ from : ValList pos xs : s3 ->
                doSlice from to xs pos $ \res -> Right cxt{stack = ValList pos res : s3}
            other1 : other2  : val : _  ->
                if isSequence val
                then typeError2 other2 "slice" "two integers as indices" other2 other1
                else typeError1 val "slice" "a string/list to operate on" val
            _ ->
                stackUnderflowError ValNoop "slice"

--
-- Perform the slicing operation with the semantics of the Stacky language.
--
doSlice :: Integer -> Integer -> [a] -> Position -> ([a] -> Result b) -> Result b
doSlice from to xs pos cont
    | to < 0 = let to' = toInteger (length xs) + to + 1
               in  doSlice from to' xs pos cont
doSlice from to xs _ cont
    | checkSlice from to xs =
        cont $ take (fromInteger (to - from)) $ drop (fromInteger from) xs
doSlice _ _ _ pos _ =
    newErrPos pos "'slice' expects '0 <= from <= to <= length'"

--
-- Check the arguments to slice for validity.
--
checkSlice :: Integer -> Integer -> [a] -> Bool
checkSlice from to xs =
    0 <= from && from <= to && fromInteger to <= length xs

-------------------------------------------------------------------------------------------------------

defOrd :: Value
defOrd =
    defOp "ord" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            ValString pos [c] : s1 ->
                Right cxt{stack = ValInt pos (toInteger $ ord c) : s1}
            ValString pos str : _ ->
                newErrPos pos ("ord expects a single-character string, got " ++ show str)
            other : _ ->
                typeError1 other "ord" "a single-character string" other
            _ ->
                stackUnderflowError ValNoop "ord"

-------------------------------------------------------------------------------------------------------

defChr :: Value
defChr =
    defOp "chr" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            ValInt pos x : s1 ->
                if x >=0 &&x < 0x110000
                then Right cxt{stack = ValString pos [chr (fromInteger x)] : s1}
                else newErrPos pos ("chr expects an integer in the Unicode interval, got " ++ show x)
            other : _ ->
                typeError1 other "chr" "an integer" other
            _ ->
                stackUnderflowError ValNoop "chr"

-------------------------------------------------------------------------------------------------------
--  Input/output operations
-------------------------------------------------------------------------------------------------------

defPrint :: Value
defPrint = ValOp noPos "print" $ \cxt@Cxt{stack = s0} ->
           case s0 of
               val : s1 -> do putStrLn $ show val
                              return $ Right cxt{stack = s1}
               _        -> return $ stackUnderflowError ValNoop "print"

-------------------------------------------------------------------------------------------------------

defPut :: Value
defPut = putVal putStr "put"

-------------------------------------------------------------------------------------------------------

defPutLn :: Value
defPutLn = putVal putStrLn "putLn"

-------------------------------------------------------------------------------------------------------

putVal :: (String -> IO ()) -> Name -> Value
putVal f n = ValOp noPos n $ \cxt@Cxt{stack = s0} ->
             case s0 of
                 val : s1 ->
                     do f $ toString val
                        return $  Right cxt{stack = s1}
                 _  ->
                     return $ stackUnderflowError ValNoop n

-------------------------------------------------------------------------------------------------------

defPrompt :: Value
defPrompt = ValOp noPos "prompt" $ \cxt@Cxt{stack = s0} ->
            case s0 of
                ValString p prompt : s1 ->
                    do str <- getLines prompt
                       return $ Right cxt{stack = ValString p str : s1}
                val : _ ->
                    return $ typeError1 val "prompt" "a string to use as a prompt" val
                _ ->
                    return $ stackUnderflowError ValNoop "prompt"

-------------------------------------------------------------------------------------------------------

defReadFile :: Value
defReadFile = ValOp noPos "readFile" $ \cxt@Cxt{stack = s0} ->
          case s0 of
              (ValString p fName) : s1 ->
                  do res <- readTheFile p fName
                     ifOk res $ \str ->
                         return $ Right cxt{stack = ValString p str : s1}
              other : _ ->
                  return $ typeError1 other "readFile" "a string file path" other
              _ ->
                  return $ stackUnderflowError ValNoop "readFile"

-------------------------------------------------------------------------------------------------------
--  Reflection/introspection operations
-------------------------------------------------------------------------------------------------------

-- defApply is defined in Interpreter

-------------------------------------------------------------------------------------------------------

defApplyList :: Value
defApplyList =
    ValOp noPos "$" $ \cxt@Cxt{stack = s0} ->
           case s0 of
              list@(ValList pos _) : s1 ->
                  runLocalValues cxt{stack = s1}
                                     [list,
                                      ValAtom pos "'",
                                      ValAtom pos "@",
                                      ValAtom pos "map"]
              _ : _ ->
                  return $ Right cxt
              _ ->
                  return $ stackUnderflowError ValNoop "$"

-------------------------------------------------------------------------------------------------------

defEval :: Value
defEval = ValOp noPos "eval" $ \cxt@Cxt{stack = s0} ->
          case s0 of
              (ValString _ str) : s1 ->
                  do let parseRes = parseLine builtIns str
                     ifOk parseRes $ \cmds -> runValues cxt{stack = s1} cmds
              other : _ ->
                  return $ typeError1 other "eval" "a string to be evaluated" other
              _ ->
                  return $ stackUnderflowError ValNoop "eval"

-------------------------------------------------------------------------------------------------------

defImport :: Value
defImport = ValOp noPos "import" $ \cxt@Cxt{stack = s0} ->
          case s0 of
              (ValString p name) : s1 ->
                  findModule name p cxt $ \path ->
                      do res  <- readTheFile p path
                         ifOk res (\str ->
                                    do let parseRes = parseFile builtIns path str
                                       ifOk parseRes (\cmds -> runValues cxt{stack = s1} cmds))
              other : _ ->
                  return $ typeError1 other "import" "a string file path" other
              _ ->
                  return $ stackUnderflowError ValNoop "readFile"

--
-- Locate the file for the module and report an error if not found
--
findModule :: String -> Position -> Cxt -> (String -> IO (Result a)) -> IO (Result a)
findModule name pos cxt handler =
    do res <- findLibModule name cxt
       case res of
           Nothing   -> return $ newErrPos pos ("import cannot find file for: '"++name++"' "++show (libPath cxt))
           Just path -> handler path


-------------------------------------------------------------------------------------------------------

defEnv :: Value
defEnv = ValOp noPos "env" $ \cxt@Cxt{envs = e0} ->
         do putStrLn $ unlines $ map (\(k,v,_) -> show k ++ " : " ++ show v) $ concat e0
            return $ Right cxt

-------------------------------------------------------------------------------------------------------

defTypeOf :: Value
defTypeOf = ValOp noPos "typeOf" $ \cxt@Cxt{stack = s0} ->
            case s0 of
                val : s1 ->
                    return $ Right cxt{stack = ValString noPos (valueType val) : s1}
                _ ->
                    return $ stackUnderflowError ValNoop "typeOf"

-------------------------------------------------------------------------------------------------------

defTypeInfo :: Value
defTypeInfo =
    ValOp noPos "typeInfo" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            val : s1 ->
                let (t, s) = valueTypeSize val
                in  return $ Right cxt{stack = ValInt noPos (toInteger s) : ValString noPos t : s1}
            _ ->
                return $ stackUnderflowError ValNoop "typeInfo"

-------------------------------------------------------------------------------------------------------

defExpectType :: Value
defExpectType =
    ValOp noPos "expectType" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            (ValList _ [ValString _ eType, ValInt _ eMin, ValInt _ eMax, ValString _ n]) : val : s1 ->
                expectType cxt eType eMin eMax n val s1
            (ValList _ [ValString _ eType, ValInt _ eMin, ValInt _ eMax]) : val : s1 ->
                expectType cxt eType eMin eMax "expectType" val s1
            other : _ ->
                return $ typeError1 other "expectType" "a type and size description" other
            _ ->
                return $ stackUnderflowError ValNoop "expectType"

--
-- Do the actual expectType operation.
--
expectType :: Cxt -> [Char] -> Integer -> Integer -> Name -> Value -> [Value] -> IO (Result Cxt)
expectType cxt eType eMin eMax name val s1 =
    let (aType, aSize) = valueTypeSize val
        eTypes = typeSet eType
        desc = ("a value of type '" ++ fmtTypeDesc eType eMin eMax ++ "'")
    in if typesMatch aType (toInteger aSize) eTypes eMin eMax
       then return $ Right cxt{stack = val : s1}
       else return $ typeError1 val name desc val

--
-- Verify that the given types match
--
typesMatch :: String -> Integer -> [String] -> Integer -> Integer -> Bool
typesMatch aType aSize eTypes eMin (-1) | aType `elem` eTypes && eMin <= aSize                 = True
typesMatch aType aSize eTypes eMin eMax | aType `elem` eTypes && eMin <= aSize && aSize < eMax = True
typesMatch _     _     _      _    _                                                           = False

--
-- Format a type description for an error message
--
fmtTypeDesc :: String -> Integer -> Integer -> String
fmtTypeDesc t sMin sMax = t ++ "(" ++ show sMin ++ "," ++ show sMax ++ ")"

--
-- Allows us to expect sequence
--
typeSet :: String -> [String]
typeSet "sequence" = ["list", "string"]
typeSet t          = [t]
                     
-------------------------------------------------------------------------------------------------------

defExpectDepth :: Value
defExpectDepth =
    ValOp noPos "expectDepth" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            (ValList _ [ValInt _ minDepth, ValString _ n]) : s1 ->
                expectDepth cxt minDepth n s1
            (ValList _ [ValInt _ minDepth]) : s1 ->
                expectDepth cxt minDepth "expectDepth" s1
            other : _ ->
                return $ typeError1 other "expectDepth" "a type and size description" other
            _ ->
                return $ stackUnderflowError ValNoop "expectDepth"

expectDepth :: Cxt -> Integer -> Name -> [Value] -> IO (Result Cxt)
expectDepth cxt minDepth name s1 =
    if toInteger (length s1) >= minDepth
    then return $ Right cxt{stack = s1}
    else return $ stackUnderflowError ValNoop name


-------------------------------------------------------------------------------------------------------

defThrow :: Value
defThrow =
    ValOp noPos "throw" $ \Cxt{stack = s0} ->
        case s0 of
            (ValList _ [ValList _ [ValString _ fn, ValInt _ l, ValInt _ c]
                       ,ValString _ msg
                       ,ValString _ name]) : _ ->
                return $ newErrPos (mk_pos fn l c) ("In '" ++ name ++ "': " ++ msg)
            (ValList pos [ValString _ msg, ValString _ name]) : _ ->
                return $ newErrPos pos ("In '" ++ name ++ "': " ++ msg)
            other : _ ->
                return $ typeError1 other "throw" "an optional position, message and name" other
            _ ->
                return $ stackUnderflowError ValNoop "throw"

mk_pos :: String -> Integer -> Integer -> Position
mk_pos fn l c = mkPos fn (fromInteger l) (fromInteger c)

-------------------------------------------------------------------------------------------------------

defCatch :: Value
defCatch =
    ValOp noPos "catch" $ \cxt@Cxt{stack = s0} ->
           case s0 of
            catchClause : tryClause : s1 ->
                do res <- runValues cxt{stack = s1} [tryClause, defApply]
                   case res of
                       Right cxt' ->
                           return $ Right cxt'
                       Left (pos, msg) ->
                           do let s2 = [ValString pos msg, pos2val pos] ++ s1
                                  cs = [catchClause, defApply]
                              runValues cxt{stack = s2} cs
            _ ->
                return $ stackUnderflowError ValNoop "catch"

pos2val :: Position -> Value
pos2val pos = ValList pos [ValString pos (fileName pos),
                           ValInt    pos (toInteger $ linePos pos),
                           ValInt    pos (toInteger $ charPos pos)]

-------------------------------------------------------------------------------------------------------

defCallPos :: Value
defCallPos =
    ValOp noPos "__CALLPOS__" $ \cxt@Cxt{stack = s0, callPos = cPos} ->
        return $ Right cxt{stack = pos2val cPos : s0}

-------------------------------------------------------------------------------------------------------
--  Local helper functions
-------------------------------------------------------------------------------------------------------

--
-- Create an entry in the environment based on the given operation's name.
--
defBI :: Value -> (Name, Value, Bool)
defBI op@(ValOp _ name _) = (name, op, False)
defBI op                  = error $ "INTERNAL ERROR: A builtin is not a ValOp: '" ++ show op ++ "'"

-------------------------------------------------------------------------------------------------------

--
-- Shorthand to define operations
--
defOp :: Name -> (Cxt -> Result Cxt) -> Value
defOp name op = ValOp noPos name $ \cxt -> return $ op cxt

-------------------------------------------------------------------------------------------------------

--
-- Haskell Bool to Stacky truthiness
--
bool2Truth :: Position -> Bool -> Value
bool2Truth pos False = ValInt pos 0
bool2Truth pos True  = ValInt pos 1

-------------------------------------------------------------------------------------------------------

--
-- Stacky truthiness to Haskell Bool
--
truth2Bool :: Value -> Bool
truth2Bool (ValInt _ 0)         = False
truth2Bool (ValFloat _ 0.0)     = False
truth2Bool (ValList _ [])       = False
truth2Bool (ValString _ "" )    = False
truth2Bool (ValAtom _ "false" ) = False
truth2Bool _                    = True

-------------------------------------------------------------------------------------------------------

--
-- Convert a value to string. In case of a string value, just return
-- the string.
--
toString :: Value -> String
toString (ValString _ str) = str
toString val               = show val

-------------------------------------------------------------------------------------------------------

--
-- Read the contents of a file and return as a result. In case of an
-- error, return an appropriate error message.
--
readTheFile :: Position -> String -> IO (Result String)
readTheFile pos fName =
    (do str <- doRead fName
        return $ Right str) `catch` (handleError pos)

--
-- If fName is "STDIN", read from stdin
--
doRead :: String -> IO String
doRead "STDIN"   = getContents
doRead fName     = readFile fName

--
-- Helper for `readTheFile`
--
handleError :: Position -> IOError -> IO (Result a)
handleError pos err =
    return $ newErrPos pos $ show err

-------------------------------------------------------------------------------------------------------
--  That's all folks!!
-------------------------------------------------------------------------------------------------------
