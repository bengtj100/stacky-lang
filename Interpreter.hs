module Interpreter (
                    interpreter,
                    runValues,
                    runValue,
                    runAtom,
                    defApply
                   ) where

import CoreTypes

-- ====================================================================================================

interpreter :: Cxt -> [Value] -> IO (Result Cxt)
interpreter = runValues


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


runLocalValues :: Cxt -> [Value] -> IO (Result Cxt)
runLocalValues cxt@Cxt{envs = es} vs =
    do res <- runValues cxt{envs = []:es} vs
       ifOk res $ \cxt1 ->
           return $ Right cxt1{envs = es}
