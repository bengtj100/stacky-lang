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
runValue cxt (ValAtom _ atom)   = runAtom cxt atom
runValue cxt (ValOp pos _ op)   = injectPos pos $ op cxt
runValue cxt@Cxt{stack = s} val = leaveVal cxt val s
                    
runAtom :: Cxt -> Name -> IO (Result Cxt)
runAtom cxt@Cxt{stack = s} atom =
    case s of
           (ValAtom pos "'") : s1 -> leaveVal cxt (ValAtom pos atom) s1
           (ValAtom pos "^") : s1 -> lookupAtom cxt atom pos s1  $ \val -> leaveVal cxt val s1
           _                      -> lookupAtom cxt atom noPos s $ \val -> runValue cxt{stack = val : s} defApply

leaveVal :: Cxt -> Value -> [Value] -> IO (Either a Cxt)
leaveVal cxt val newStack =
    return $ Right cxt{stack = val : newStack}
                            
lookupAtom :: Cxt -> Name -> Position -> [Value] -> (Value -> IO (Either Error Cxt)) -> IO (Either Error Cxt)
lookupAtom cxt atom pos s cont = 
    case lookupEnv cxt atom of
        Nothing                -> leaveVal cxt (ValAtom pos atom) s
        Just (ValOp pos' _ op) -> injectPos pos' $ op cxt
        Just val               -> cont val
                            

injectPos :: Position -> IO (Result a) -> IO (Result a)
injectPos pos iop =
    iop >>= \res ->
               return $ case res of
                            l@(Left (p, err)) | isNoPos p  -> Left (pos, err)
                                              | otherwise  -> l
                            right                          -> right


-- ====================================================================================================

defApply :: Value
defApply =
    ValOp noPos "@" $ \cxt@Cxt{stack = s0} ->
           case s0 of
              ValList _ cmds : s1 ->
                  runLocalValues cxt{stack = s1} cmds
              ValAtom _ atom : s1 ->
                  runAtom cxt{stack = s1} atom
              _ : _ ->
                  return $ Right cxt
              _ ->
                  return $ stackUnderflowError ValNoop "@"


runLocalValues :: Cxt -> [Value] -> IO (Result Cxt)
runLocalValues cxt@Cxt{envs = es} vs =
    do res <- runValues cxt{envs = []:es} vs
       ifOk res $ \cxt1 ->
           return $ Right cxt1{envs = es}

