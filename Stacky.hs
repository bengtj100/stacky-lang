module Stacky (repl) where
                     
import Data.Char
import System.IO
    
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
    (ValInt i1)    == (ValInt i2)    = i1 == i2
    (ValAtom a1)   == (ValAtom a2)   = a1 == a2
    (ValString s1) == (ValString s2) = s1 == s2
    (ValStack st1) == (ValStack st2) = st1 == st2
    (ValOp n1 _)   == (ValOp n2 _)   = n1 == n2
    _              == _              = False


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
fmtString "" = ""
fmtString ('"' : str) = '\\' : '"' : fmtString str
fmtString (c : str)   = c : fmtString str
                           
-- ====================================================================================================

type Error = String

-- ====================================================================================================

type Result a = Either Error a

stackUnderflowError :: Name -> Result a
stackUnderflowError name = Left $ "Stack underflow in operation: '" ++ name ++ "'"

typeError :: Name -> String -> Value -> Value -> Result Value
typeError name comment x y =
    Left ("Operation '" ++ name ++ "' expects " ++ comment
          ++ ", got '" ++ valueType x ++ "' and '" ++ valueType y ++ "', got '"
          ++ show x ++ "' and '" ++ show y ++ "'")

-- ====================================================================================================

type Stack = [Value]

newStack :: Stack
newStack = []

fmtStack :: Stack -> String
fmtStack = unwords . map show . reverse

-- ====================================================================================================

type Env = [(Name, Value)]

newEnv :: Env
newEnv = builtIns

insertEnv :: Cxt -> Name -> Value -> Result Cxt
insertEnv Cxt{envs = env:_} key _ | key `elem` map fst env =
    Left $ "Redefining name: '" ++ key ++ "'"
insertEnv cxt@Cxt{envs = env:es} key val =
    Right $ cxt{envs=((key, val) : env) : es}
insertEnv _ _ _ = error "INSERTENV CALLED WITH EMPTY ENV STACK! THIS SHOULD NEVER HAPPEN!"
    

lookupEnv :: Cxt -> Name -> Maybe Value
lookupEnv Cxt{envs = es} key = lookup key (concat es)

getBuiltIn :: Name -> Maybe Value
getBuiltIn name = lookup name builtIns

-- ====================================================================================================

data Cxt = Cxt{ stack :: Stack, envs :: [Env] }

initCxt :: Cxt
initCxt = Cxt{stack = newStack, envs = [newEnv]}
         
-- ====================================================================================================

repl :: IO ()
repl = r initCxt
       where r cxt =
                 do -- putStrLn "STACKY V1.0 (c) Bengt Johansson"
                    putStr "> "
                    hFlush stdout
                    line    <- getLine
                    result  <- interpreter cxt line
                    nextCxt <- either (\err    -> do { printError err; return cxt; })
                                      (\newCxt -> do printStack newCxt
                                                     return newCxt)
                                      result
                    r nextCxt
                               
printStack :: Cxt -> IO ()
printStack Cxt{stack = elems} = putStrLn $ fmtStack elems

printError :: Error -> IO ()
printError = putStrLn . ("ERROR: "++)

-- ====================================================================================================

ifOk :: Result a -> (a -> IO (Result b)) -> IO (Result b)
ifOk (Left err) _    = return $ Left err
ifOk (Right ok) cont = cont ok


interpreter :: Cxt -> String -> IO (Result Cxt)
interpreter cxt line =
    ifOk (parser line) $ \cmds -> runValues cxt cmds

runLocalValues :: Cxt -> [Value] -> IO (Result Cxt)
runLocalValues cxt@Cxt{envs = es} vs =
    do res <- runValues cxt{envs = []:es} vs
       ifOk res $ \cxt1 ->
           return $ Right cxt1{envs = es}


runValues :: Cxt -> [Value] -> IO (Result Cxt)
runValues cxt [] =
    return $ Right cxt
runValues cxt (v : vs) =
    do res <- runValue cxt v
       ifOk res $ flip runValues vs

runValue :: Cxt -> Value -> IO (Result Cxt)
runValue cxt (ValAtom atom)     = runAtom cxt atom
runValue cxt (ValOp _ op)       = op cxt
runValue cxt@Cxt{stack = s} val = return $ Right cxt{stack = val : s}
                    
runAtom :: Cxt -> Name -> IO (Result Cxt)
runAtom cxt@Cxt{stack = s} atom =
    case s of
           ValAtom "'" : s1 ->
               return $ Right cxt{stack = ValAtom atom : s1}
           _ ->
               case lookupEnv cxt atom of
                   Nothing ->
                       return $ Right cxt{stack = ValAtom atom : s}
                   Just (ValOp _ op) -> 
                       do res <- op cxt
                          ifOk res $ \cxt1 ->
                              return $ Right cxt1
                   Just val ->
                       runValue cxt{stack = val : s} defApply
                        

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
numBinOp name _ x           y           = typeError name "numerical arguments of same type" x y

cmpBinOp :: Name -> (Integer -> Integer -> Bool) -> Value -> Value -> Result Value
cmpBinOp _    f (ValInt v1) (ValInt v2) = Right $ bool2Truth $ f v1 v2
cmpBinOp name _ x           y           = typeError name "comparable arguments of same type" x y

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
                               Left ("Type error: Expected an atom as key for stash, found: '"
                                     ++ valueType key ++ "' with value '"
                                    ++ show key ++ "'")
                           _ ->
                               stackUnderflowError ";"

defApply :: Value
defApply =
    ValOp "@" $ \cxt@Cxt{stack = s0} ->
           case s0 of
              ValStack cmds : s1 ->
                  runLocalValues cxt{stack = s1} cmds
              ValAtom atom : s1 ->
                  runAtom cxt{stack = s1} atom
              _ : _ ->
                  return $ Right cxt
              _ ->
                  return $ stackUnderflowError "@"

defAppend :: Value
defAppend  =
    defOp "++" $ \cxt@Cxt{stack = s0} ->
        case s0 of
            ValStack s1 : ValStack s2 : s3 ->
                Right cxt{stack = ValStack (s2 ++ s1) : s3}
            ValString str1 : ValString str2 : s3 ->
                Right cxt{stack = ValString (str2 ++ str1) : s3}
            v1 : v2 : _ ->
                Left ("(++) expects either two stacks or strings, got '"
                      ++ valueType v2 ++ "' and '" ++ valueType v1 ++ "'")
            _ ->
                stackUnderflowError "++"

-- ====================================================================================================

parser :: String -> Result [Value]
parser str =
    do (cmds, rest) <- lexer str
       if rest /= ""
       then Left ("Trailing input garbage: '" ++ rest ++ "'")
       else parseCmds cmds

parseCmds :: [Value] -> Result [Value]
parseCmds []                   = return []
parseCmds (ValAtom "[" : cmds) =
    do (sCmds, rest) <- parseStack cmds
       cmds'         <- parseCmds rest
       return $ ValStack sCmds : cmds'
parseCmds (inhibitor@(ValAtom "'") : atom@(ValAtom _) : cmds) =
    do cmds' <- parseCmds cmds
       return $ inhibitor : atom : cmds'
parseCmds (ValAtom atom : cmds) =
    do cmds' <- parseCmds cmds
       return $ (parseAtom atom) : cmds'
parseCmds (cmd : cmds) =
    do cmds' <- parseCmds cmds
       return $ cmd : cmds'

parseAtom :: Name -> Value
parseAtom atom =
    case getBuiltIn atom of
        Nothing -> ValAtom atom
        Just op -> op

parseStack :: [Value] -> Result ([Value], [Value])
parseStack []                   = Left "Missing end of stack marker. (']')"
parseStack (ValAtom "]" : cmds) = return ([], cmds)
parseStack (ValAtom "[" : cmds) = do (sCmds, rest)   <- parseStack cmds
                                     (sCmds', rest') <- parseStack rest
                                     return (ValStack sCmds : sCmds', rest')
parseStack (cmd : cmds)         = do (sCmds, rest) <- parseStack cmds
                                     return (cmd : sCmds, rest)


dropWhite :: String -> String
dropWhite = dropWhile isSpace

lexer :: String -> Result ([Value], String)
lexer = lx . dropWhite
        where lx "" =
                  Right ([], "")
              lx str =           
                  do (t, next) <- token str
                     (ts, rest) <- lexer next
                     Right $ (t:ts, rest)

token :: String -> Result (Value, String)
token str@('-' : c :_) | isDigit c = intToken str
token str@(c : _)      | isDigit c = intToken str
token ('"' : str)                  = strToken str
token str                          = atomToken str

strToken :: String -> Result(Value, String)
strToken str = case rest of
                   ""       -> Left $ "Unterminated string constant: '" ++ val ++ "'"
                   _ : rest' -> Right (ValString val, rest')
               where (val, rest) = tok str
                     tok ""                   = ("", "")
                     tok rest'@('"' : _)      = ("", rest')
                     tok ('\\' : '"' : str')  = let (val', rest') = tok str'
                                                in ('"' : val', rest')
                     tok (c : str')           = let (val', rest') = tok str'
                                                in (c: val', rest')
intToken :: String -> Result (Value, String)
intToken str = Right (ValInt x, rest) where (x, rest) = head $ reads str

atomToken :: String -> Result (Value, String)
atomToken (c:d:rest) | [c,d] `elem` ops2 = Right (ValAtom [c,d], rest)
atomToken (c:rest)   | c     `elem` ops1 = Right (ValAtom [c],   rest)
atomToken (c:str)    | isAlpha c         = let (cs, rest) = span (\x -> isAlphaNum x || x=='_') str
                                           in Right (ValAtom (c:cs), rest)
atomToken (c:_)                          = Left ("Unknown character: '" ++ [c] ++ "'")
atomToken ""                             = Left "Out of input data"
                                                              
ops1 :: [Char]
ops1 = ['\'', '[', ']'] ++ [ c | ([c], _) <- builtIns, not $ isAlphaNum c ]

ops2 :: [String]
ops2 = [ [c,d] | ([c,d], _) <- builtIns, not $ isAlphaNum c ]



