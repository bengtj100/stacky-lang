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
-- This module contains the interpreter proper of the Stacky
-- language. It deals with operations that are intrinsic to the
-- language. Built-in operations can be found in the BuiltIns module.
--
-------------------------------------------------------------------------------------------------------

module Interpreter (
                    interpreter,
                    runValues,
                    defApply,
                    runLocalValues
                   ) where

-- Base modules 
import CoreTypes( Cxt(..),     lookupEnv, pushLocal, popLocal, clearLocal
                , Value(..)
                , Result,      ifOk
                , Name
                , Error,       newErrPos, stackUnderflowError)

import Position(Position(..), noPos, isNoPos)

-------------------------------------------------------------------------------------------------------
-- Main interpreter API functions and their helpers
-------------------------------------------------------------------------------------------------------

--
-- Main interpreter entrypoiny
--
interpreter :: Cxt -> [Value] -> IO (Result Cxt)
interpreter = runValues

--
-- Execute a list of operations in a given context.
-- Return the updated context or and error.
--
runValues :: Cxt -> [Value] -> IO (Result Cxt)
runValues cxt []       = return $ Right cxt
runValues cxt [v]      = do runValue cxt v True
runValues cxt (v : vs) = do res <- runValue cxt v False
                            ifOk res $ flip runValues vs

--
-- Execute a single operation in a given context.
-- Return the updated context or and error.
--
runValue :: Cxt -> Value -> Bool -> IO (Result Cxt)
runValue cxt                (ValAtom pos atom) l = runAtom pos cxt atom l
runValue cxt                (ValOp   pos _ op) _ = injectPos pos $ op cxt
runValue cxt@Cxt{stack = s} val                _ = leaveVal cxt val s

--
-- Evaluate an atom by looking it up in the environment.
-- If inhibitors are present on the stack, act accordlingly.
--
runAtom :: Position -> Cxt -> Name -> Bool -> IO (Result Cxt)
runAtom pos cxt@Cxt{stack = s} atom l =
    case s of
           (ValAtom _ "'") : s1 -> leaveVal cxt (ValAtom pos atom) s1
           (ValAtom _ "^") : s1 -> lookupAtom cxt atom pos s1 $ \val -> leaveVal cxt val s1
           _                    -> lookupAtom cxt atom pos s  $ \val -> runApply cxt{callPos = pos} val l

runApply :: Cxt -> Value -> Bool -> IO (Result Cxt)
runApply cxt (ValList _   cmds) False = runLocalValues cxt cmds
runApply cxt (ValList _   cmds) True  = runValues (clearLocal cxt) cmds
runApply cxt (ValAtom pos atom) l     = runAtom pos cxt atom l
runApply cxt (ValOp   pos _ op) _     = injectPos pos $ op cxt
runApply cxt val                _     = leaveVal cxt val (stack cxt)

-------------------------------------------------------------------------------------------------------
-- Auxilliary public functions
-------------------------------------------------------------------------------------------------------

--
-- This is the definition of the apply ('@') operation. It is located
-- here to avoid circular dependencies between the interpreter and the
-- BuiltIns module.
--
defApply :: Value
defApply =
    ValOp noPos "@" $ \cxt@Cxt{stack = s0} ->
           case s0 of
              val : s1 -> runApply cxt{stack = s1} val False
              _        -> return $ stackUnderflowError ValNoop "@"

--
-- Evaluate a list of operations in its own local environment, which
-- is discarded upon return. This is howe get local variables.
--
runLocalValues :: Cxt -> [Value] -> IO (Result Cxt)
runLocalValues cxt vs = do res <- runValues (pushLocal cxt) vs
                           ifOk res $ \cxt1 ->
                               return $ Right $ popLocal cxt1

-------------------------------------------------------------------------------------------------------
-- Local helper functions
-------------------------------------------------------------------------------------------------------

--
-- This leaves a new value in a (possibly new stack). Essentially a
-- macro for the incantations needed to return a succesful
-- IO (Result Cxt)
--
leaveVal :: Cxt -> Value -> [Value] -> IO (Either a Cxt)
leaveVal cxt val newStack =
    return $ Right cxt{stack = val : newStack}

--
-- Look up the value of an atom in the environment and continue
-- processing it in `cont`.
--
lookupAtom :: Cxt ->
              Name ->
              Position ->
              [Value] ->
              (Value -> IO (Either Error Cxt)) -> IO (Either Error Cxt)
lookupAtom cxt atom pos s cont = 
    case lookupEnv cxt atom of
        Nothing  | atom `elem` ["'", "^"] -> leaveVal cxt (ValAtom pos atom) s
                 | otherwise           -> return $ newErrPos pos ("Undefined name: '" ++ atom ++ "'")
        Just val -> cont val
                            
--
-- Insert a position into an error message, if none is present.
--
injectPos :: Position -> IO (Result a) -> IO (Result a)
injectPos pos iop =
    iop >>= \res ->
               return $ case res of
                            l@(Left (p, err)) | isNoPos p  -> Left (pos, err)
                                              | otherwise  -> l
                            right                          -> right

-------------------------------------------------------------------------------------------------------
--  That's all folks!!
-------------------------------------------------------------------------------------------------------

