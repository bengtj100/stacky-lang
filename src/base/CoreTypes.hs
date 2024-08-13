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

module CoreTypes (
                  ---- Cxt ----
                  Cxt(..),
                  initCxt,

                  ---- Env ----
                  Env,
                  insertEnv,
                  lookupEnv,

                  ---- Stack ----
                  Stack,
                  printStack,

                  ---- Value ----
                  Value(..),
                  valueType, valueTypeSize,
                  isComparable, isSequence,
                  getValPos,

                  ---- Operation ----
                  Operation,

                  ---- Result ----
                  Result,
                  stackUnderflowError,
                  typeError1,
                  typeError2,
                  ifOk,

                  ---- Error ----
                  Error,
                  newError, newErrPos,
                  printError, printErrorWithProgname,

                  ---- Name ----
                  Name

                 ) where

-- System modules
import System.Environment(getProgName)

-- Local modules
import Position(Position, fmtPosition, noPos)

-------------------------------------------------------------------------------------------------------
--  Cxt - The interpreter execution context
-------------------------------------------------------------------------------------------------------

data Cxt = Cxt{ stack :: Stack
              , envs  :: [Env]
              , libPath :: [String]
              }

initCxt :: Env -> Cxt
initCxt initEnv = Cxt{ stack = newStack
                     , envs = [newEnv ++ initEnv]
                     , libPath = []
                     }


-------------------------------------------------------------------------------------------------------
--  Env - The interpreter name bindings
-------------------------------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------------------------------
--  Stack - The main evaluation stack
-------------------------------------------------------------------------------------------------------

type Stack = [Value]

newStack :: Stack
newStack = []

fmtStack :: Stack -> String
fmtStack = unwords . map show . reverse

printStack :: Cxt -> IO ()
printStack Cxt{stack = elems} = putStrLn $ "[ " ++ fmtStack elems ++ " <]"

-------------------------------------------------------------------------------------------------------
--  Value - The values we operate on
-------------------------------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------------------------------
--  Operation - The type of built-in operations
-------------------------------------------------------------------------------------------------------

type Operation = Cxt -> IO (Result Cxt)

-------------------------------------------------------------------------------------------------------
--  Result - The result of an operation
-------------------------------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------------------------------
--  Error - Representation of errors
-------------------------------------------------------------------------------------------------------

type Error = (Position, String)

printError :: Error -> IO ()
printError (pos, msg) = putStrLn (fmtPosition pos ++"ERROR: " ++ msg)

printErrorWithProgname :: Error -> IO ()
printErrorWithProgname (pos, msg) =
    do pName <- getProgName
       putStrLn (fmtPosition pos ++ pName ++ ": " ++ msg)

newError :: Value -> String -> Result a
newError val err = newErrPos (getValPos val) err

newErrPos :: Position -> String -> Result a
newErrPos pos err = Left (pos, err)

-------------------------------------------------------------------------------------------------------
--  That's all folks
-------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------
--  Name - Representation of names in the language
-------------------------------------------------------------------------------------------------------

type Name = String
                   
-------------------------------------------------------------------------------------------------------
--  That's all folks!
-------------------------------------------------------------------------------------------------------
