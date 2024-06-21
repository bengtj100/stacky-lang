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
                  Error,
                  Result,
                  Stack,
                  Env,
                  Cxt(..),
                  Parser,
                  -- Value functions
                  getValInt,
                  isComparable,
                  -- Error functions
                  printError,
                  -- Result functions
                  stackUnderflowError,
                  typeError1,
                  typeError2,
                  ifOk,
                  -- Stack functions
                  printStack,
                  -- Env functions
                  insertEnv,
                  lookupEnv,
                  -- Cxt functions
                  initCxt
                 ) where

    
-- ====================================================================================================

type Name = String
    
type Operation = Cxt -> IO (Result Cxt)

data Value = ValInt Integer
           | ValAtom Name
           | ValString String
           | ValStack Stack
           | ValOp Name Operation

instance Show Value where
    show (ValInt i)       = show i
    show (ValAtom a)      = a
    show (ValString str)  = "\"" ++ fmtString str ++ "\""
    show (ValStack s)     = "[ " ++ (fmtStack $ reverse s) ++ " ]"
    show (ValOp name _)  = "{"++name++"}"

instance Eq Value where
    (ValInt i1)    == (ValInt i2)    =  i1 == i2
    (ValAtom a1)   == (ValAtom a2)   =  a1 == a2
    (ValString s1) == (ValString s2) =  s1 == s2
    (ValStack st1) == (ValStack st2) = st1 == st2
    (ValOp n1 _)   == (ValOp n2 _)   =  n1 == n2
    (ValAtom n1)   == (ValOp n2 _)   =  n1 == n2
    (ValOp n1 _)   == (ValAtom n2)   =  n1 == n2
    _              == _              = False

instance Ord Value where
    (ValInt i1)    <= (ValInt i2)    =  i1 <= i2
    (ValAtom a1)   <= (ValAtom a2)   =  a1 <= a2
    (ValString s1) <= (ValString s2) =  s1 <= s2
    (ValOp s1 _)   <= (ValOp s2 _)   =  s1 <= s2
    (ValOp s1 _)   <= (ValAtom s2)   =  s1 <= s2
    (ValAtom s1)   <= (ValOp s2 _)   =  s1 <= s2
    (ValStack st1) <= (ValStack st2) = st1 <= st2
    _              <= _              = False

isComparable :: Value -> Value -> Bool
isComparable (ValInt _)    (ValInt _)    = True
isComparable (ValString _) (ValString _) = True
isComparable (ValStack _)  (ValStack _)  = True
isComparable (ValAtom _)   (ValAtom _)   = True
isComparable (ValOp _ _)   (ValAtom _)   = True
isComparable (ValAtom _)   (ValOp _ _)   = True
isComparable (ValOp _ _)   (ValOp _ _)   = True
isComparable _             _             = False

valueType :: Value -> String
valueType (ValInt _)    = "integer"
valueType (ValAtom _)   = "atom"
valueType (ValString _) = "string"
valueType (ValStack _)  = "stack"
valueType (ValOp _ _)   = "atom"
                          
getValInt :: Value -> Result Integer
getValInt (ValInt i) = Right i
getValInt v          = Left ("Expected an int got: '" ++ valueType v ++ "'")

fmtString :: String -> String
fmtString ""           = ""
fmtString ('"' : str)  = '\\' : '"' : fmtString str
fmtString ('\n' : str) = '\\' : 'n' : fmtString str
fmtString ('\r' : str) = '\\' : 'r' : fmtString str
fmtString ('\t' : str) = '\\' : 't' : fmtString str
fmtString ('\\' : str) = '\\' : '\\' : fmtString str
fmtString (c : str)    = c : fmtString str
                           
-- ====================================================================================================

type Error = String

printError :: Error -> IO ()
printError = putStrLn . ("ERROR: "++)

-- ====================================================================================================

type Result a = Either Error a

stackUnderflowError :: Name -> Result a
stackUnderflowError name = Left $ "Stack underflow in operation: '" ++ name ++ "'"

typeError1 :: Name -> String -> Value -> Result a
typeError1 name comment x =
    Left ("Operation '" ++ name ++ "' expects " ++ comment
          ++ ", got '" ++ valWType x ++ "'")

typeError2 :: Name -> String -> Value -> Value -> Result a
typeError2 name comment x y =
    Left ("Operation '" ++ name ++ "' expects " ++ comment
          ++ ", got '" ++ valWType x ++ "' and '" ++ valWType y ++ "'")

valWType :: Value -> String
valWType x = show x ++ " : " ++ valueType x

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
printStack Cxt{stack = elems} = putStrLn $ fmtStack elems

-- ====================================================================================================

type Env = [(Name, Value)]

newEnv :: Env
newEnv = []

insertEnv :: Cxt -> Name -> Value -> Result Cxt
insertEnv Cxt{envs = env:_} key _ | key `elem` map fst env =
    Left $ "Redefining name: '" ++ key ++ "'"
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
