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

module CoreTypes (
                  Name,

                  Operation,

                  Value(..),
                  valueType, valueTypeSize,
                  isComparable, isSequence,
                  getValPos,
                  valFloatFun,
                  valAdd, valSub, valMult, valDiv, valRem,
                  valPow, valFloor, valFloat,

                  Error,
                  newError, newErrPos,
                  printError, printErrorWithProgname,

                  Result,
                  stackUnderflowError,
                  typeError1,
                  typeError2,
                  ifOk,

                  Stack,
                  printStack,

                  Env,
                  insertEnv,
                  lookupEnv,

                  Cxt(..),
                  initCxt,

                  Parser,

                  Position(..),
                  initPos,
                  nextPos,
                  nextPosStr,
                  incPosLine,
                  incPosChar,
                  noPos, isNoPos
                 ) where

import System.Environment(getProgName)
import Text.Read(readMaybe)
    
-- ====================================================================================================

type Name = String
    
type Operation = Cxt -> IO (Result Cxt)

data Value = ValInt    Position Integer
           | ValFloat  Position Double
           | ValAtom   Position Name
           | ValString Position String
           | ValList   Position [Value]
           | ValOp     Position Name Operation
           | ValNoop

instance Show Value where
    show (ValInt    _ i     ) = show i
    show (ValFloat  _ f     ) = show f
    show (ValAtom   _ a     ) = a
    show (ValString _ str   ) = "\"" ++ fmtString str ++ "\""
    show (ValList   _ xs    ) = fmtList xs
    show (ValOp     _ name _) = "{"++name++"}"
    show (ValNoop           ) = "{}"

instance Eq Value where
    (ValInt    _ i1)   == (ValInt    _ i2)   =  i1 == i2
    (ValFloat  _ f1)   == (ValFloat  _ f2)   =  f1 == f2
    (ValInt    _ i1)   == (ValFloat  _ f2)   =  fromIntegral i1 == f2
    (ValFloat  _ f1)   == (ValInt    _ i2)   =  f1 == fromIntegral i2
    (ValAtom   _ a1)   == (ValAtom   _ a2)   =  a1 == a2
    (ValString _ s1)   == (ValString _ s2)   =  s1 == s2
    (ValList   _ xs1)  == (ValList   _ xs2)  = xs1 == xs2
    (ValOp     _ n1 _) == (ValOp     _ n2 _) =  n1 == n2
    (ValAtom   _ n1)   == (ValOp     _ n2 _) =  n1 == n2
    (ValOp     _ n1 _) == (ValAtom   _ n2)   =  n1 == n2
    _                  == _                  =  False

instance Ord Value where
    (ValInt    _ i1)   <= (ValInt    _ i2)   =  i1 <= i2
    (ValFloat  _ f1)   <= (ValFloat  _ f2)   =  f1 <= f2
    (ValInt    _ i1)   <= (ValFloat  _ f2)   =  fromIntegral i1 <= f2
    (ValFloat  _ f1)   <= (ValInt    _ i2)   =  f1 <= fromIntegral i2
    (ValAtom   _ a1)   <= (ValAtom   _ a2)   =  a1 <= a2
    (ValString _ s1)   <= (ValString _ s2)   =  s1 <= s2
    (ValOp     _ s1 _) <= (ValOp     _ s2 _) =  s1 <= s2
    (ValOp     _ s1 _) <= (ValAtom   _ s2)   =  s1 <= s2
    (ValAtom   _ s1)   <= (ValOp     _ s2 _) =  s1 <= s2
    (ValList   _ xs1)  <= (ValList   _ xs2)  = xs1 <= xs2
    _                  <= _                  =  False

isComparable :: Value -> Value -> Bool
isComparable (ValInt _ _)    (ValInt _ _)    = True
isComparable (ValFloat _ _)  (ValFloat _ _)  = True
isComparable (ValInt _ _)    (ValFloat _ _)  = True
isComparable (ValFloat _ _)  (ValInt _ _)    = True
isComparable (ValString _ _) (ValString _ _) = True
isComparable (ValList _ _)   (ValList _ _)   = True
isComparable (ValAtom _ _)   (ValAtom _ _)   = True
isComparable (ValOp _ _ _)   (ValAtom _ _)   = True
isComparable (ValAtom _ _)   (ValOp _ _ _)   = True
isComparable (ValOp _ _ _)   (ValOp _ _ _)   = True
isComparable _             _                 = False

isSequence :: Value -> Bool
isSequence (ValList _ _)   = True
isSequence (ValString _ _) = True
isSequence _               = False

valueType :: Value -> String
valueType (ValInt _ _)    = "integer"
valueType (ValFloat _ _)  = "float"
valueType (ValAtom _ _)   = "atom"
valueType (ValString _ _) = "string"
valueType (ValList _ _)   = "list"
valueType (ValOp _ _ _)   = "atom"
valueType (ValNoop)       = "noop"

valueTypeSize :: Value -> (String, Int)
valueTypeSize (ValInt _ _)      = ("integer", 1)
valueTypeSize (ValFloat _ _)    = ("float", 1)
valueTypeSize (ValAtom _ _)     = ("atom", 1)
valueTypeSize (ValString _ str) = ("string", length str)
valueTypeSize (ValList _ xs)    = ("list", length xs)
valueTypeSize (ValOp _ _ _)     = ("atom", 1)
valueTypeSize (ValNoop)         = ("noop", 0)
                          
getValPos :: Value -> Position
getValPos (ValInt pos _)    = pos
getValPos (ValFloat pos _)  = pos
getValPos (ValAtom pos _)   = pos
getValPos (ValString pos _) = pos
getValPos (ValList pos _)   = pos
getValPos (ValOp pos _ _)   = pos
getValPos (ValNoop)         = noPos
                              
fmtList :: [Value] -> String
fmtList xs = "[" ++ unwords (map show xs) ++ "]"

fmtString :: String -> String
fmtString ""           = ""
fmtString ('"' : str)  = '\\' : '"' : fmtString str
fmtString ('\n' : str) = '\\' : 'n' : fmtString str
fmtString ('\r' : str) = '\\' : 'r' : fmtString str
fmtString ('\t' : str) = '\\' : 't' : fmtString str
fmtString ('\\' : str) = '\\' : '\\' : fmtString str
fmtString (c : str)    = c : fmtString str

-- ----------------------------------------------------------------------------------------------------

type IntOp   = Integer -> Integer -> Integer
type FloatOp = Double  -> Double  -> Double

valAdd :: Value -> Value -> Result Value
valAdd x y = valOp "+" (+) (+) x y

valSub :: Value -> Value -> Result Value
valSub x y = valOp "-" (-) (+) x y

valMult :: Value -> Value -> Result Value
valMult x y = valOp "*" (*) (*) x y

valDiv :: Value -> Value -> Result Value
valDiv x y = valOp "/" div (/) x y

valRem :: Value -> Value -> Result Value
valRem x y = valOp "/" rem floatRem x y

floatRem :: Double -> Double -> Double
floatRem fx fy = fromIntegral (x `rem` y)
                 where x = floor fx :: Integer
                       y = floor fy :: Integer

valPow :: Value -> Value -> Result Value
valPow x y = valOp "pow" intPow (**) x y

intPow :: Integer -> Integer -> Integer
intPow x n | n < 1          = 1
           | n `rem` 2 == 1 = x * intPow x (n-1)
           | otherwise      = x2 * x2
           where
               x2 = intPow x (n `div` 2)

valOp :: Name -> IntOp -> FloatOp -> Value -> Value -> Result Value
valOp _    iop _   (ValInt pos x)   (ValInt _ y)   = Right $ ValInt   pos (x `iop` y)
valOp _    _   fop (ValFloat pos x) (ValFloat _ y) = Right $ ValFloat pos (x `fop` y)
valOp _    _   fop (ValInt pos x)   (ValFloat _ y) = Right $ ValFloat pos (fromIntegral x `fop` y)
valOp _    _   fop (ValFloat pos x) (ValInt _ y)   = Right $ ValFloat pos (x `fop` fromIntegral y)
valOp name _   _   vx               vy             = typeError2 vx name "numerical arguments" vx vy

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
                   
-- ====================================================================================================

type Error = (Position, String)

printError :: Error -> IO ()
printError (pos, msg) = putStrLn (fmtPosition pos ++"ERROR: " ++ msg)

printErrorWithProgname :: Error -> IO ()
printErrorWithProgname (pos, msg) =
    do pName <- getProgName
       putStrLn (fmtPosition pos ++ pName ++ ": " ++ msg)
                                    
fmtPosition :: Position -> String
fmtPosition pos | isNoPos pos  = ""
                | otherwise    = fileName pos ++ ":"
                                 ++ show (linePos pos) ++ ":"
                                 ++ show (charPos pos) ++ ": "

newError :: Value -> String -> Result a
newError val err = newErrPos (getValPos val) err

newErrPos :: Position -> String -> Result a
newErrPos pos err = Left (pos, err)

-- ====================================================================================================

type Result a = Either Error a

stackUnderflowError :: Value -> Name -> Result a
stackUnderflowError val name = newError val $ "Stack underflow in operation: '" ++ name ++ "'"

typeError1 :: Value ->  Name -> String -> Value -> Result a
typeError1 val name comment x =
    newError val ("Operation '" ++ name ++ "' expects " ++ comment
                  ++ ", got '" ++ valWType x ++ "'")

typeError2 :: Value -> Name -> String -> Value -> Value -> Result a
typeError2 val name comment x y =
    newError val ("Operation '" ++ name ++ "' expects " ++ comment
                  ++ ", got '" ++ valWType x ++ "' and '" ++ valWType y ++ "'")

valWType :: Value -> String
valWType x = show x ++ " : " ++ t ++ "(" ++ show s ++ ")"
             where (t, s) = valueTypeSize x

ifOk :: Result a -> (a -> IO (Result b)) -> IO (Result b)
ifOk (Left err) _    = return $ Left err
ifOk (Right ok) cont = cont ok


-- ====================================================================================================

type Stack = [Value]

newStack :: Stack
newStack = []

fmtStack :: Stack -> String
fmtStack = unwords . map show . reverse

printStack :: Cxt -> IO ()
printStack Cxt{stack = elems} = putStrLn $ "[ " ++ fmtStack elems ++ " <]"

-- ====================================================================================================

type Env = [(Name, Value)]

newEnv :: Env
newEnv = []

insertEnv :: Cxt -> Name -> Value -> Result Cxt
insertEnv Cxt{envs = env:_} key _ | key `elem` map fst env =
    newError ValNoop $ "Redefining name: '" ++ key ++ "'"
insertEnv cxt@Cxt{envs = env:es} key val =
    Right $ cxt{envs=((key, val) : env) : es}
insertEnv _ _ _ = error "INSERTENV CALLED WITH EMPTY ENV STACK! THIS SHOULD NEVER HAPPEN!"
    

lookupEnv :: Cxt -> Name -> Maybe Value
lookupEnv Cxt{envs = es} key = lookup key (concat es)

-- ====================================================================================================

data Cxt = Cxt{ stack :: Stack, envs :: [Env] }

initCxt :: Env -> Cxt
initCxt initEnv = Cxt{stack = newStack, envs = [newEnv ++ initEnv]}

-- ====================================================================================================

type Parser = [Value] -> Result [Value]

-- ====================================================================================================

data Position = Pos{ fileName :: String, linePos :: Int, charPos :: Int } deriving (Show, Read, Eq)

initPos :: String -> Position
initPos fname = Pos{fileName = fname, linePos = 0, charPos = 0}


nextPos :: Position -> Char -> Position
nextPos p@Pos{linePos = l} '\n' = p{linePos = l + 1, charPos = 0}
nextPos p                  '\r' = p{charPos = 0}
nextPos p@Pos{charPos = c} '\t' = p{charPos = calcTab c}
nextPos p@Pos{charPos = c} _    = p{charPos = c + 1}


nextPosStr :: Position -> String -> Position
nextPosStr = foldl nextPos

incPosLine :: Position -> Int -> Position
incPosLine p@Pos{linePos = l} i = p{linePos = l + i}

incPosChar :: Position -> Int -> Position
incPosChar p@Pos{charPos = l} i = p{charPos = l + i}
                                  
calcTab :: Int -> Int
calcTab c = c + (8 - c `mod` 8)

noPos :: Position
noPos = Pos{fileName = "", linePos = -1, charPos = -1}

isNoPos :: Position -> Bool
isNoPos Pos{fileName = ""} = True
isNoPos _                  = False
                             
