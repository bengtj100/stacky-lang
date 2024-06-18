module Stacky (repl) where
                     
import Data.Char
import System.IO
    
-- ====================================================================================================

data Value = ValInt Integer
           | ValAtom String
           | ValString String
           | ValStack Stack   deriving (Show, Eq)

valueType :: Value -> String
valueType (ValInt _)     = "integer"
valueType (ValAtom _)    = "atom"
valueType (ValString _)  = "string"
valueType (ValStack _)   = "stack"

getValInt :: Value -> Result Integer
getValInt (ValInt i) = Right i
getValInt v          = Left ("Expected an int got: '" ++ valueType v ++ "'")

fmtValue :: Value -> String
fmtValue (ValInt i)       = show i
fmtValue (ValAtom a)      = a
fmtValue (ValString str)  = "\"" ++ fmtString str ++ "\""
fmtValue (ValStack s)     = "[ " ++ (fmtStack $ reverse s) ++ " ]"

fmtString :: String -> String
fmtString "" = ""
fmtString ('"' : str) = '\\' : '"' : fmtString str
fmtString (c : str)   = c : fmtString str
                           
-- ====================================================================================================

type Error = String

-- ====================================================================================================

type Result a = Either Error a

-- ====================================================================================================

type Stack = [Value]

newStack :: Stack
newStack = []

fmtStack :: Stack -> String
fmtStack = unwords . map fmtValue . reverse

-- ====================================================================================================

type Env = [(String, Operation)]

newEnv :: Env
newEnv =
    [
     ("+", runBinIntOp "+" (+)),
     ("-", runBinIntOp "-" (-)),
     ("*", runBinIntOp "*" (*)),
     ("/", runBinIntOp "/" div),
     ("=", runBinCmpOp "=" (==)),
     ("<", runBinCmpOp "<" (<)),
     (">", runBinCmpOp ">" (>)),
     ("<=", runBinCmpOp "<=" (<=)),
     (">=", runBinCmpOp ">=" (>=)),
     ("<>", runBinCmpOp "<>" (/=)),

     ("~", runUnBoolOp "~" not),
     ("and", runBinBoolOp "and" (&&)),
     ("or", runBinBoolOp "or" (||)),
                                     
     (";", runStash),
     ("@", runApply),
     ("?", runCond),

     ("drop", runDrop),
     ("swap", runSwap),
     ("rot",  runRot),
     ("over", runOver),
     ("dup",  runDup),
     ("clear", runClear),
     ("depth", runDepth),
     ("print", runPrint),

     ("++", runAppend)
    ]

insertEnv :: Cxt -> String -> Operation -> Result Cxt
insertEnv Cxt{envs = env:_} key _ | key `elem` map fst env =
    Left $ "Redefining name: '" ++ key ++ "'"
insertEnv cxt@Cxt{envs = env:es} key op =
    Right $ cxt{envs=((key, op) : env) : es}
insertEnv _ _ _ = error "INSERTENV CALLED WITH EMPTY ENV STACK! THIS SHOULD NEVER HAPPEN!"
    

lookupEnv :: Cxt -> String -> Maybe Operation
lookupEnv Cxt{envs = es} key = lookup key (concat es)

-- ====================================================================================================

data Cxt = Cxt{ stack :: Stack, envs :: [Env] }

initCxt :: Cxt
initCxt = Cxt{stack = newStack, envs = [newEnv]}
         
-- ====================================================================================================

newtype Operation = Op (Cxt -> IO (Result Cxt))

seqOp :: Operation -> Operation -> Operation
seqOp (Op op1) (Op op2) =
    Op $ \cxt1 ->
        do res1 <- op1 cxt1
           ifOk res1 $ \cxt2 ->
               op2 cxt2

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
runValue cxt@Cxt{stack = s} val = return $ Right cxt{stack = val : s}

runAtom :: Cxt -> String -> IO (Result Cxt)
runAtom cxt@Cxt{stack = s} atom =
    case s of
           ValAtom "'" : s1 ->
               return $ Right cxt{stack = ValAtom atom : s1}
           _ ->
               case lookupEnv cxt atom of
                   Nothing      ->
                       return $ Right cxt{stack = ValAtom atom : s}
                   Just (Op op) -> 
                       do res <- op cxt
                          ifOk res $ \cxt1 ->
                              return $ Right cxt1
                        

runBinIntOp :: String -> (Integer -> Integer -> Integer) -> Operation
runBinIntOp name f = runBinOp $ numBinOp name f

runBinCmpOp :: String -> (Integer -> Integer -> Bool) -> Operation
runBinCmpOp name f = runBinOp $ cmpBinOp name f

runUnBoolOp :: String -> (Bool -> Bool) -> Operation
runUnBoolOp name f = runUnOp $ boolUnOp name f

runBinBoolOp :: String -> (Bool -> Bool -> Bool) -> Operation
runBinBoolOp name f = runBinOp $ boolBinOp name f

runBinOp :: (Value -> Value -> Result Value) -> Operation
runBinOp f =
    runOp $ \cxt ->
        case stack cxt of
            x : y : stack1 -> do { val <- f y x; return cxt{stack = val : stack1}; }
            _              -> Left "Stack underflow"
                         
runUnOp :: (Value -> Result Value) -> Operation
runUnOp f = 
    runOp $ \cxt ->
        case cxt of
            cxt1@Cxt{stack = x : s} -> do { val <- f x; return cxt1{stack = val : s}; }
            _                       -> Left "Stack underflow"

numBinOp :: String -> (Integer -> Integer -> Integer) -> Value -> Value -> Result Value
numBinOp _    f (ValInt v1) (ValInt v2) = Right $ ValInt $ f v1 v2
numBinOp name _ x           y           = typeError name "numerical arguments of same type" x y

cmpBinOp :: String -> (Integer -> Integer -> Bool) -> Value -> Value -> Result Value
cmpBinOp _    f (ValInt v1) (ValInt v2) = Right $ bool2Truth $ f v1 v2
cmpBinOp name _ x           y           = typeError name "comparable arguments of same type" x y

boolBinOp :: String -> (Bool -> Bool -> Bool) -> Value -> Value -> Result Value
boolBinOp _ f v1 v2 = Right $ bool2Truth $ f (truth2Bool v1) (truth2Bool v2)

boolUnOp :: String -> (Bool -> Bool) -> Value -> Result Value
boolUnOp _ f v1 = Right $ bool2Truth $ f (truth2Bool v1)

typeError :: String -> String -> Value -> Value -> Result Value
typeError name comment x y =
    Left ("Operation '" ++ name ++ "' expects " ++ comment
          ++ ", got '" ++ valueType x ++ "' and '" ++ valueType y ++ "', got '"
          ++ fmtValue x ++ "' and '" ++ fmtValue y ++ "'")


bool2Truth :: Bool -> Value
bool2Truth False = ValInt 0
bool2Truth True  = ValInt 1

truth2Bool :: Value -> Bool
truth2Bool (ValInt 0)    = False
truth2Bool (ValStack []) = False
truth2Bool _             = True

-- ====================================================================================================

runOp :: (Cxt -> Result Cxt) -> Operation
runOp op = Op $ \cxt -> return $ op cxt


runCond :: Operation
runCond =
    Op $ \cxt@Cxt{stack = s0} ->
        case s0 of
           elsePart : thenPart : predicate : s1 ->
               do result <- runValues cxt{stack = s1} [predicate, ValAtom "@"]
                  ifOk result $ \cxt1@Cxt{stack = s2} ->
                      ifOk (getValInt $ head s2) $ \predInt ->
                          runValues cxt1{stack = tail s2} $
                              if predInt /= 0
                                  then [thenPart, ValAtom "@"]
                                  else [elsePart, ValAtom "@"]
           _ ->
               return $ Left "Stack underflow"

runPush :: Value -> Operation
runPush val = runOp $ \cxt@Cxt{stack = s} -> Right cxt{stack = val : s}

runDrop :: Operation
runDrop = runOp $ \cxt@Cxt{stack = s0} ->
              case s0 of
                  _ : s1 -> Right cxt{stack = s1}
                  _      -> Left "Stack underflow"

runSwap :: Operation
runSwap = runOp $ \cxt@Cxt{stack = s0} ->
              case s0 of
                  x : y : s1 -> Right cxt{stack = y : x : s1}
                  _          -> Left "stack underflow"


runRot :: Operation
runRot = runOp $ \cxt@Cxt{stack = s0} ->
             case s0 of
                 x : y : z : s1 -> Right cxt{stack = z : y : x : s1}
                 _              -> Left "stack underflow"

runOver :: Operation
runOver = runOp $ \cxt@Cxt{stack = s0} ->
              case s0 of
                  x : y : s1 -> Right cxt{stack = y : x : y : s1}
                  _          -> Left "stack underflow"

runDup :: Operation
runDup = runOp $ \cxt@Cxt{stack = s0} ->
             case s0 of
                 x : s1 -> Right cxt{stack = x : x : s1}
                 _      -> Left "stack underflow"

runClear :: Operation
runClear = runOp $ \cxt -> Right cxt{stack = []}

runDepth :: Operation
runDepth = runOp $ \cxt@Cxt{stack = s} ->
               let
                   depth = (ValInt $ toInteger $ length s)
               in
                   Right cxt{stack = depth : s}
                         
runPrint :: Operation
runPrint = Op $ \cxt@Cxt{stack = s0} ->
          case s0 of
              val : s1 -> do putStrLn $ fmtValue val
                             return $  Right cxt{stack = s1}
              _        -> return $ Left "stack underflow"
                                 
runStash :: Operation
runStash = runOp $ \cxt@Cxt{stack = s0} ->
                       case s0 of
                           ValAtom key : val : s1 ->
                               let
                                   op = seqOp (runPush val) runApply
                               in
                                   insertEnv cxt{stack = s1} key op 
                           key : _ : _ ->
                               Left ("Type error: Expected an atom as key for stash, found: '"
                                     ++ valueType key ++ "' with value '"
                                    ++ fmtValue key ++ "'")
                           _ ->
                               Left "Stack underflow"

runApply :: Operation
runApply =
    Op $ \cxt@Cxt{stack = s0} ->
           case s0 of
              ValStack cmds : s1 ->
                  runLocalValues cxt{stack = s1} cmds
              ValAtom atom : s1 ->
                  runAtom cxt{stack = s1} atom
              _ : _ ->
                  return $ Right cxt
              _ ->
                  return $ Left "Stack underflow"

runAppend :: Operation
runAppend  =
    runOp $ \cxt@Cxt{stack = s0} ->
        case s0 of
            ValStack s1 : ValStack s2 : s3 ->
                Right cxt{stack = ValStack (s2 ++ s1) : s3}
            ValString str1 : ValString str2 : s3 ->
                Right cxt{stack = ValString (str2 ++ str1) : s3}
            v1 : v2 : _ ->
                Left ("(++) expects either two stacks or strings, got '"
                      ++ valueType v2 ++ "' and '" ++ valueType v1 ++ "'")
            _ ->
                Left "stack underflow"

-- ====================================================================================================

parser :: String -> Result [Value]
parser str =
    do (cmds, rest) <- lexer str
       if rest /= ""
       then Left ("Trailing input garbage: '" ++ rest ++ "'")
       else parseCmds cmds

parseCmds :: [Value] -> Result [Value]
parseCmds []                   = return []
parseCmds (ValAtom "[" : cmds) = do (sCmds, rest) <- parseStack cmds
                                    cmds'         <- parseCmds rest
                                    return $ ValStack sCmds : cmds'
parseCmds (cmd : cmds)         = do cmds' <- parseCmds cmds
                                    return $ cmd : cmds'

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
ops1 = ['\'', '[', ']'] ++ [ c | ([c], _) <- newEnv, not $ isAlphaNum c ]

ops2 :: [String]
ops2 = [ [c,d] | ([c,d], _) <- newEnv, not $ isAlphaNum c ]



