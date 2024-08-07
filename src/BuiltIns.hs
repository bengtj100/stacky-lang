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
                 parseLine
                ) where

import Data.Char
import Control.Exception

import CoreTypes
import Position
import Interpreter
import InputOutput
import FrontEnd
    
-- ====================================================================================================

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
               defStash, defCond,

               -- Stack operations
               defDrop,  defSwap,  defRot,  defLRot,  defOver,  defDup, defClear, defDepth,
               defNDrop, defNSwap, defNRot, defNLRot, defNOver, defNDup,

               -- String/list operations
               defAppend, defLength,
               defToList, defFromList,
               defToString, defFromString,
               defReverse, defToStr, defSlice,
               defChr, defOrd,
                       
               -- Input/output operations
               defPrint, defPut, defPutLn, defInput, defPrompt, defReadFile,

               -- Reflection/introspection operations
               defApply, defApplyList, defEval, defImport, defEnv, defTypeOf,
               defTypeInfo, defExpectType, defExpectDepth, defThrow
              ]

defMath :: Name -> (Double -> Double) -> Value
defMath name f = defUnOp  name $ valFloatFun name f

defBI :: Value -> (Name, Value)
defBI op@(ValOp _ name _) = (name, op)
defBI op                  = error $ "INTERNAL ERROR: A builtin is not a ValOp: '" ++ show op ++ "'"


defBinCmpOp :: Name -> (Value -> Value -> Bool) -> Value
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
            _              -> stackUnderflowError ValNoop name
                         
defUnOp :: Name -> (Value -> Result Value) -> Value
defUnOp name f = 
    defOp name $ \cxt ->
        case cxt of
            cxt1@Cxt{stack = x : s} -> do { val <- f x; return cxt1{stack = val : s}; }
            _                       -> stackUnderflowError ValNoop name

cmpBinOp :: Name -> (Value -> Value -> Bool) -> Value -> Value -> Result Value
cmpBinOp name f x y | isComparable x y = Right $ bool2Truth (getValPos x) $ f x y
                    | otherwise        = typeError2 x name "comparable arguments" x y

boolBinOp :: Name -> (Bool -> Bool -> Bool) -> Value -> Value -> Result Value
boolBinOp _ f v1 v2 = Right $ bool2Truth (getValPos v1) $ f (truth2Bool v1) (truth2Bool v2)

boolUnOp :: Name -> (Bool -> Bool) -> Value -> Result Value
boolUnOp _ f v1 = Right $ bool2Truth (getValPos v1) $ f (truth2Bool v1)


bool2Truth :: Position -> Bool -> Value
bool2Truth pos False = ValInt pos 0
bool2Truth pos True  = ValInt pos 1

truth2Bool :: Value -> Bool
truth2Bool (ValInt _ 0)      = False
truth2Bool (ValFloat _ 0.0)  = False
truth2Bool (ValList _ [])    = False
truth2Bool (ValString _ "" ) = False                             
truth2Bool _                 = True

-- ====================================================================================================

defOp :: Name -> (Cxt -> Result Cxt) -> Value
defOp name op = ValOp noPos name $ \cxt -> return $ op cxt


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

safeHead :: [Value] -> Value
safeHead []    = ValInt noPos 0
safeHead (x:_) = x

safeTail :: [Value] -> [Value]
safeTail []     = []
safeTail (_:xs) = xs
                  
defDrop :: Value
defDrop = defOp "drop" $ \cxt@Cxt{stack = s0} ->
              case s0 of
                  _ : s1 -> Right cxt{stack = s1}
                  _      -> stackUnderflowError ValNoop "drop"

defSwap :: Value
defSwap = defOp "swap" $ \cxt@Cxt{stack = s0} ->
              case s0 of
                  x : y : s1 -> Right cxt{stack = y : x : s1}
                  _          -> stackUnderflowError ValNoop "swap"


defRot :: Value
defRot = defOp "rot" $ \cxt@Cxt{stack = s0} ->
             case s0 of
                 x : y : z : s1 -> Right cxt{stack = z : x : y : s1}
                 _              -> stackUnderflowError ValNoop "rot"

defLRot :: Value
defLRot = defOp "lrot" $ \cxt@Cxt{stack = s0} ->
             case s0 of
                 x : y : z : s1 -> Right cxt{stack = y : z : x : s1}
                 _              -> stackUnderflowError ValNoop "rot"

defOver :: Value
defOver = defOp "over" $ \cxt@Cxt{stack = s0} ->
              case s0 of
                  x : y : s1 -> Right cxt{stack = y : x : y : s1}
                  _          -> stackUnderflowError ValNoop "over"

defDup :: Value
defDup = defOp "dup" $ \cxt@Cxt{stack = s0} ->
             case s0 of
                 x : s1 -> Right cxt{stack = x : x : s1}
                 _      -> stackUnderflowError ValNoop "dup"

defClear :: Value
defClear = defOp "clear" $ \cxt -> Right cxt{stack = []}

defDepth :: Value
defDepth = defOp "depth" $ \cxt@Cxt{stack = s} ->
               let
                   depth = (ValInt noPos $ toInteger $ length s)
               in
                   Right cxt{stack = depth : s}                         

defNDrop :: Value
defNDrop = nStackOp "ndrop" $ \_ st -> st
    
defNSwap :: Value
defNSwap = nStackOp "nswap" $ \pfix st -> reverse pfix ++ st

defNRot :: Value
defNRot = nStackOp "nrot" $ \pfix st ->
          if null pfix
             then st
             else (last pfix) : (init pfix) ++ st

defNLRot :: Value
defNLRot = nStackOp "nlrot" $ \pfix st ->
           if null pfix
              then st
              else (tail pfix) ++ (head pfix : st)

defNOver :: Value
defNOver = nStackOp "nover" $ \pfix st ->
           if null pfix
              then st
              else (last pfix) : pfix ++ st

defNDup :: Value
defNDup = nStackOp "ndup" $ \pfix st -> pfix ++ pfix ++ st

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


defStash :: Value
defStash = defOp ";" $ \cxt@Cxt{stack = s0} ->
                       case s0 of
                           ValAtom _ key : val : s1 ->
                               insertEnv cxt{stack = s1} key val 
                           key : _ : _ ->
                               typeError1 key ";" "an atom as key for" key
                           _ ->
                               stackUnderflowError ValNoop ";"

defAppend :: Value
defAppend  =
    defOp "++" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            ValList p xs : ValList _ ys : s3 ->
                Right cxt{stack = ValList p (ys ++ xs) : s3}
            ValString p str1 : ValString _ str2 : s3 ->
                Right cxt{stack = ValString p (str2 ++ str1) : s3}
            v1 : v2 : _ ->
                typeError2 v1 "++" "either two stacks or strings" v2 v1
            _ ->
                stackUnderflowError ValNoop "++"

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

defToStr :: Value
defToStr  =
    defOp "toStr" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            val : s3 ->
                Right cxt{stack = ValString (getValPos val) (show val) : s3}
            _ ->
                stackUnderflowError ValNoop "toStr"

defToList :: Value
defToList =
    nStackOp "toList" $ \pfix st -> ValList noPos (reverse pfix) : st

defToString :: Value
defToString =
    nStackOp "toString" $ \pfix st ->
        ValString noPos (concat $ map toString $ reverse pfix) : st

toString :: Value -> String
toString (ValString _ str) = str
toString val               = show val

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
                               
doSlice :: Integer -> Integer -> [a] -> Position -> ([a] -> Result b) -> Result b
doSlice from to xs pos cont
    | to < 0 = let to' = toInteger (length xs) + to + 1
               in  doSlice from to' xs pos cont
doSlice from to xs _ cont
    | checkSlice from to xs =
        cont $ take (fromInteger (to - from)) $ drop (fromInteger from) xs
doSlice _ _ _ pos _ =
    newErrPos pos "'slice' expects '0 <= from <= to <= length'"
    
checkSlice :: Integer -> Integer -> [a] -> Bool
checkSlice from to xs =
    0 <= from && from <= to && fromInteger to <= length xs
                   
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

defPrint :: Value
defPrint = ValOp noPos "print" $ \cxt@Cxt{stack = s0} ->
           case s0 of
               val : s1 -> do putStrLn $ show val
                              return $ Right cxt{stack = s1}
               _        -> return $ stackUnderflowError ValNoop "print"

defPut :: Value
defPut = putVal putStr "put"

defPutLn :: Value
defPutLn = putVal putStrLn "putLn"

putVal :: (String -> IO ()) -> Name -> Value
putVal f n = ValOp noPos n $ \cxt@Cxt{stack = s0} ->
             case s0 of
                 val : s1 ->
                     do f $ toString val
                        return $  Right cxt{stack = s1}
                 _  ->
                     return $ stackUnderflowError ValNoop n

defInput :: Value
defInput = ValOp noPos "input" $ \cxt@Cxt{stack = s0} ->
           do str <- getLines "? "
              return $ Right cxt{stack = ValString noPos str : s0}

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

defEval :: Value
defEval = ValOp noPos "eval" $ \cxt@Cxt{stack = s0} ->
          case s0 of
              (ValString _ str) : s1 ->
                  do let parseRes = parseLine builtIns str
                     ifOk parseRes $ \cmds -> interpreter cxt{stack = s1} cmds
              other : _ ->
                  return $ typeError1 other "eval" "a string to be evaluated" other
              _ ->
                  return $ stackUnderflowError ValNoop "eval"

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

defImport :: Value
defImport = ValOp noPos "import" $ \cxt@Cxt{stack = s0} ->
          case s0 of
              (ValString p fName) : s1 ->
                  do res <- readTheFile p fName
                     ifOk res (\str ->
                               do let parseRes = parseFile builtIns fName str
                                  ifOk parseRes (\cmds -> interpreter cxt{stack = s1} cmds))
              other : _ ->
                  return $ typeError1 other "import" "a string file path" other
              _ ->
                  return $ stackUnderflowError ValNoop "readFile"

readTheFile :: Position -> String -> IO (Result String)
readTheFile pos fName =
    (do str <- readFile fName
        return $ Right str) `catch` (handleError pos)

handleError :: Position -> IOError -> IO (Result a)
handleError pos err =
    return $ newErrPos pos $ show err


defEnv :: Value
defEnv = ValOp noPos "env" $ \cxt@Cxt{envs = e0} ->
         do putStrLn $ unlines $ map (\(k,v) -> show k ++ " : " ++ show v) $ concat e0
            return $ Right cxt

defTypeOf :: Value
defTypeOf = ValOp noPos "typeOf" $ \cxt@Cxt{stack = s0} ->
            case s0 of
                val : s1 ->
                    return $ Right cxt{stack = ValString noPos (valueType val) : s1}
                _ ->
                    return $ stackUnderflowError ValNoop "typeOf"

defTypeInfo :: Value
defTypeInfo =
    ValOp noPos "typeInfo" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            val : s1 ->
                let (t, s) = valueTypeSize val
                in  return $ Right cxt{stack = ValInt noPos (toInteger s) : ValString noPos t : s1}
            _ ->
                return $ stackUnderflowError ValNoop "typeInfo"

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
                       
expectType :: Cxt -> [Char] -> Integer -> Integer -> Name -> Value -> [Value] -> IO (Result Cxt)
expectType cxt eType eMin eMax name val s1 =
    let (aType, aSize) = valueTypeSize val
        eTypes = typeSet eType
        desc = ("a value of type '" ++ fmtTypeDesc eType eMin eMax ++ "'")
    in if typesMatch aType (toInteger aSize) eTypes eMin eMax
       then return $ Right cxt{stack = val : s1}
       else return $ typeError1 val name desc val
    
fmtTypeDesc :: String -> Integer -> Integer -> String
fmtTypeDesc t sMin sMax = t ++ "(" ++ show sMin ++ "," ++ show sMax ++ ")"

typeSet :: String -> [String]
typeSet "sequence" = ["list", "string"]
typeSet t          = [t]
                     
typesMatch :: String -> Integer -> [String] -> Integer -> Integer -> Bool
typesMatch aType aSize eTypes eMin (-1) | aType `elem` eTypes && eMin <= aSize                 = True
typesMatch aType aSize eTypes eMin eMax | aType `elem` eTypes && eMin <= aSize && aSize < eMax = True
typesMatch _     _     _      _    _                                                           = False

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


defThrow :: Value
defThrow =
    ValOp noPos "throw" $ \Cxt{stack = s0} ->
        case s0 of
            (ValList pos [ValString _ msg, ValString _ name]) : _ ->
                return $ newErrPos pos ("In '" ++ name ++ "': " ++ msg)
            other : _ ->
                return $ typeError1 other "throw" "a type and size description" other
            _ ->
                return $ stackUnderflowError ValNoop "throw"

-- ====================================================================================================
